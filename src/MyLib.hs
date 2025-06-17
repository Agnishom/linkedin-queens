module MyLib (
    -- someFunc,
    -- solutions,
    -- exampleProblem,
) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

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

data Remaining = Satisfied | AvailableCandidates Int
    deriving (Show, Eq)

decrease :: Remaining -> Remaining
decrease (AvailableCandidates n)
    | n > 0 = AvailableCandidates (n - 1)
    | otherwise = error "Cannot decrease Remaining below 0"
decrease Satisfied = Satisfied

data Partial = Partial {
    attempts :: BoardOf Attempt,
    rowCandidates :: Map Row Remaining,
    columnCandidates :: Map Column Remaining,
    colorCandidates :: Map Color Remaining
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
        Nothing -> Map.adjust decrease x partial.rowCandidates
        Just _ -> partial.rowCandidates
    newColumnCandidates = case currentCellValue of
        Nothing -> Map.adjust decrease y partial.columnCandidates
        Just _ -> partial.columnCandidates
    newColorCandidates = case (currentCellValue, cellColor) of
        (Nothing, Just color) -> Map.adjust decrease color partial.colorCandidates
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

openCells :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
openCells problem partial = foldr interleave empty [ pure (x, y) | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1], Map.lookup (x, y) partial.attempts == Nothing ]

outOfCandidates :: Partial -> Bool
outOfCandidates partial = outOfRowCandidates || outOfColumnCandidates || outOfColorCandidates
    where
    outOfRowCandidates = any isZero (Map.elems partial.rowCandidates)
    outOfColumnCandidates = any isZero (Map.elems partial.columnCandidates)
    outOfColorCandidates = any isZero (Map.elems partial.colorCandidates)
    isZero (AvailableCandidates 0) = True
    isZero _ = False

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
    ifte 
        (openCells problem partial)
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
    rowCandidates = Map.fromList [(r, AvailableCandidates problem.size) | r <- [0 .. problem.size - 1]],
    columnCandidates = Map.fromList [(c, AvailableCandidates problem.size) | c <- [0 .. problem.size - 1]],
    colorCandidates = Map.fromList [(color, AvailableCandidates problem.size) | color <- [0 .. problem.size - 1]]
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
-- Timeout
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


someFunc :: IO ()
someFunc = putStrLn "someFunc"
