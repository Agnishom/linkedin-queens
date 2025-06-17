module MyLib (
    -- someFunc,
    -- solutions,
    -- exampleProblem,
) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortBy, minimumBy)

type Color = Int
type Row = Int
type Column = Int

type BoardOf a = Map (Row, Column) a

data Problem = Problem {
    colors :: BoardOf Color,
    size :: Int
}
    deriving (Show, Eq)

data Attempt = HasQueen | Eliminated
    deriving (Show, Eq)

data Remaining a = Satisfied | AvailableCandidates (Set a)
    deriving (Show, Eq)

remove :: Ord a => a -> Remaining a -> Remaining a
remove x (AvailableCandidates s)
    | (Set.size s) > 0 = AvailableCandidates (Set.delete x s)
    | otherwise = error "Cannot remove from an empty set"
remove _ Satisfied = Satisfied

data Partial = Partial {
    attempts :: BoardOf Attempt,
    rowCandidates :: Map Row (Remaining Column),
    columnCandidates :: Map Column (Remaining Row),
    colorCandidates :: Map Color (Remaining (Row, Column))
} deriving (Show, Eq)

insertIfAbsent :: Ord k => k -> a -> Map k a -> Map k a
insertIfAbsent = Map.insertWith (flip const)

eliminate :: (Row, Column) -> Problem -> Partial -> Partial
eliminate (x, y) problem partial = 
    Partial { attempts = newAttempts, 
              rowCandidates = newRowCandidates,
              columnCandidates = newColumnCandidates,
              colorCandidates = newColorCandidates }
    where
    cellColor = Map.lookup (x, y) problem.colors
    currentCellValue = Map.lookup (x, y) partial.attempts
    newAttempts = case currentCellValue of
        Nothing -> Map.insert (x, y) Eliminated partial.attempts
        _ -> partial.attempts
    newRowCandidates = case currentCellValue of
        Nothing -> Map.adjust (remove y) x partial.rowCandidates
        Just _ -> partial.rowCandidates
    newColumnCandidates = case currentCellValue of
        Nothing -> Map.adjust (remove x) y partial.columnCandidates
        Just _ -> partial.columnCandidates
    newColorCandidates = case (currentCellValue, cellColor) of
        (Nothing, Just color) -> Map.adjust (remove (x, y)) color partial.colorCandidates
        _ -> partial.colorCandidates

elimCorners :: (Row, Column) -> Problem -> Partial -> Partial
elimCorners (x, y) problem partial = foldr (.) id elimFns $ partial
    where
    elimFns = 
        [ eliminate (x', y') problem | 
            x' <- [x - 1, x + 1], 
            y' <- [y - 1, y + 1], 
            x' >= 0, x' < problem.size, y' >= 0, y' < problem.size
        ]

elimColumn :: Column -> Problem -> Partial -> Partial
elimColumn j problem partial = foldr elimCell partial [0 .. problem.size - 1]
    where
    elimCell i = eliminate (i, j) problem

elimRow :: Row -> Problem -> Partial -> Partial
elimRow i problem partial = foldr elimCell partial [0 .. problem.size - 1]
    where
    elimCell j = eliminate (i, j) problem

elimColor :: Color -> Problem -> Partial -> Partial
elimColor color problem = foldr (.) id elimFns
    where
        elimFns = [ eliminate (x, y) problem | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1], Map.lookup (x, y) problem.colors == Just color ]


placeQueen :: Problem -> (Row, Column) -> Partial -> Partial
placeQueen problem (x, y) partial = 
          elimCorners (x, y) problem  
        . elimColumn y problem
        . elimRow x problem
        . maybe id (flip elimColor problem) cellColor 
        $ Partial {
            attempts = newAttempts,
            rowCandidates = newRowCandidates,
            columnCandidates = newColumnCandidates,
            colorCandidates = newColorCandidates
            }
    where
    newAttempts = Map.insert (x, y) HasQueen partial.attempts
    newRowCandidates = Map.insert x Satisfied partial.rowCandidates
    newColumnCandidates = Map.insert y Satisfied partial.columnCandidates
    newColorCandidates = case cellColor of
        Just color -> Map.insert color Satisfied partial.colorCandidates
        Nothing -> error "No color found for the cell"
    cellColor = Map.lookup (x, y) problem.colors

outOfCandidates :: Partial -> Bool
outOfCandidates partial = outOfRowCandidates || outOfColumnCandidates || outOfColorCandidates
    where
    outOfRowCandidates = any isOut (Map.elems partial.rowCandidates)
    outOfColumnCandidates = any isOut (Map.elems partial.columnCandidates)
    outOfColorCandidates = any isOut (Map.elems partial.colorCandidates)
    isOut (AvailableCandidates s) = Set.size s == 0
    isOut _ = False

data Candidate = 
      RowCandidate Row (Set Column)
    | ColumnCandidate Column (Set Row)
    | ColorCandidate Color (Set (Row, Column))

compareCandidates :: Candidate -> Candidate -> Ordering
compareCandidates cand1 cand2 = compare (size cand1) (size cand2)
    where
    size (RowCandidate _ s) = Set.size s
    size (ColumnCandidate _ s) = Set.size s
    size (ColorCandidate _ s) = Set.size s

allCandidates :: Partial -> [Candidate]
allCandidates partial = 
    [ RowCandidate r s | (r, AvailableCandidates s) <- Map.toList partial.rowCandidates ] ++
    [ ColumnCandidate c s | (c, AvailableCandidates s) <- Map.toList partial.columnCandidates ] ++
    [ ColorCandidate color s | (color, AvailableCandidates s) <- Map.toList partial.colorCandidates ]

expandCandidates :: Problem -> Partial -> Candidate -> [(Row, Column)]
expandCandidates problem partial (RowCandidate r s) = [(r, c) | c <- Set.toList s, checkRowCandidate (r, c) problem partial]
expandCandidates problem partial (ColumnCandidate c s) = [(r, c) | r <- Set.toList s, checkColumnCandidate (r, c) problem partial]
expandCandidates problem partial (ColorCandidate color s) = [(i, j) | (i, j) <- Set.toList s, checkColorCandidate (i, j) problem partial]

checkRowCandidate :: (Row, Column) -> Problem -> Partial -> Bool
checkRowCandidate (r, c) problem partial = columnCheck && colorCheck
    where
    columnCheck = case Map.lookup c partial.columnCandidates of
        Just (AvailableCandidates s) -> Set.member r s
        _ -> False
    cellColor = problem.colors ! (r, c)
    colorCheck = case Map.lookup cellColor partial.colorCandidates of
        Just (AvailableCandidates s) -> Set.member (r, c) s
        _ -> False 

checkColumnCandidate :: (Row, Column) -> Problem -> Partial -> Bool
checkColumnCandidate (r, c) problem partial = rowCheck && colorCheck
    where
    rowCheck = case Map.lookup r partial.rowCandidates of
        Just (AvailableCandidates s) -> Set.member c s
        _ -> False
    cellColor = problem.colors ! (r, c)
    colorCheck = case Map.lookup cellColor partial.colorCandidates of
        Just (AvailableCandidates s) -> Set.member (r, c) s

checkColorCandidate :: (Row, Column) -> Problem -> Partial -> Bool
checkColorCandidate (r, c) problem partial = rowCheck && columnCheck
    where
    rowCheck = case Map.lookup r partial.rowCandidates of
        Just (AvailableCandidates s) -> Set.member c s
        _ -> False
    columnCheck = case Map.lookup c partial.columnCandidates of
        Just (AvailableCandidates s) -> Set.member r s
        _ -> False

genCandidates :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
genCandidates problem partial
    | null candidates = empty
    | otherwise = let cand = minimumBy compareCandidates candidates in foldr interleave empty $ [ pure (x, y) | (x, y) <- expandCandidates problem partial cand ]
    where
        candidates = allCandidates partial

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
    ifte 
        (genCandidates problem partial)
        (\ (x, y) -> do
            let newPartial = placeQueen problem (x, y) partial
            solve problem newPartial)
        (guard (not (outOfCandidates partial)) >> pure partial)

queenView :: Partial -> [(Int, Int)]
queenView partial = [(x, y) | ((x, y), HasQueen) <- Map.toList partial.attempts]

solutions :: (MonadLogic m) => Problem -> m (Partial)
solutions problem = do 
    candidate <- solve problem (mkPartial problem)
    let queens = queenView candidate
    -- Completeness: ensure enough queens were placed
    guard (length queens == problem.size)
    pure candidate

mkPartial :: Problem -> Partial
mkPartial problem = Partial {
    attempts = Map.empty,
    rowCandidates = Map.fromList [(r, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | r <- [0 .. problem.size - 1]],
    columnCandidates = Map.fromList [(c, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | c <- [0 .. problem.size - 1]],
    colorCandidates = Map.fromList [(color, AvailableCandidates (Set.fromList [(x, y) | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1]])) | color <- [0 .. problem.size - 1]]
}

mkProblem :: [[Color]] -> Problem
mkProblem colorsList = Problem {
    colors = Map.fromList [((i, j), color) | (i, row) <- zip [0..] colorsList, (j, color) <- zip [0..] row],
    size = length colorsList
}

-- Sample problem on https://www.linkedin.com/games/queens
-- Pass
exampleProblem :: Problem
exampleProblem = mkProblem
    [ [0, 1, 1, 1, 1]
    , [2, 3, 2, 1, 2]
    , [2, 3, 2, 1, 2]
    , [2, 2, 2, 4, 4]
    , [2, 2, 2, 2, 2]
    ]

-- https://queensgame.vercel.app/level/1
-- Pass
exampleProblem2 :: Problem
exampleProblem2 = mkProblem
    [ [0, 0, 1, 1, 1, 2, 2, 2]
    , [0, 3, 1, 3, 1, 4, 2, 2]
    , [0, 3, 1, 3, 1, 2, 2, 2]
    , [0, 3, 3, 3, 1, 5, 6, 2]
    , [0, 3, 3, 3, 1, 5, 6, 6]
    , [0, 3, 7, 3, 1, 5, 6, 6]
    , [7, 3, 7, 3, 1, 5, 5, 6]
    , [7, 7, 7, 7, 6, 6, 6, 6]
    ]

-- https://queensgame.vercel.app/community-level/1
-- Pass
exampleProblem3 :: Problem
exampleProblem3 = mkProblem
    [ [0, 0, 0, 1, 2, 3]
    , [0, 0, 0, 1, 2, 3]
    , [1, 1, 1, 1, 2, 3]
    , [1, 1, 4, 2, 2, 3]
    , [5, 4, 4, 2, 2, 3]
    , [5, 5, 4, 4, 2, 2]
    ]

-- https://queensgame.vercel.app/community-level/3
-- Pass
exampleProblem4 :: Problem
exampleProblem4 = mkProblem
    [ [0, 0, 0, 1, 1, 1, 1]
    , [0, 0, 1, 1, 1, 1, 1]
    , [2, 2, 2, 1, 3, 3, 3]
    , [2, 4, 4, 4, 3, 3, 3]
    , [2, 4, 4, 4, 3, 3, 3]
    , [2, 4, 4, 4, 5, 5, 6]
    , [2, 2, 2, 2, 5, 5, 5]
    ]

exampleProblem5 :: Problem
exampleProblem5 = mkProblem
    [ [0, 1, 1, 1, 2, 1, 1, 3]
    , [0, 0, 0, 1, 2, 2, 1, 3]
    , [1, 1, 1, 1, 1, 2, 1, 3]
    , [4, 4, 4, 1, 1, 1, 1, 3]
    , [4, 1, 1, 1, 1, 1, 1, 1]
    , [1, 1, 1, 1, 5, 1, 1, 6]
    , [7, 7, 1, 5, 5, 1, 6, 6]
    , [7, 7, 1, 5, 1, 1, 1, 6]
    ]


someFunc :: IO ()
someFunc = putStrLn "someFunc"
