module MyLib (
    someFunc,
    solutions,
    exampleProblem,
) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

type Color = Int

type BoardOf a = Map (Int, Int) a

data Problem = Problem {
    colors :: BoardOf Color,
    size :: Int
}
    deriving (Show, Eq)

data Attempt = HasQueen | Eliminated
    deriving (Show, Eq)

type Partial = BoardOf Attempt

insertIfAbsent :: Ord k => k -> a -> Map k a -> Map k a
insertIfAbsent = Map.insertWith (flip const)

placeQueen :: Problem -> (Int, Int) -> BoardOf Attempt -> BoardOf Attempt
placeQueen problem (x, y) board = 
          elimCorners problem.size (x, y) 
        . elimColumn problem.size y 
        . elimRow problem.size x 
        . maybe id (elimColor problem) cellColor 
        $ newBoard
    where
    newBoard = Map.insert (x, y) HasQueen board
    cellColor = Map.lookup (x, y) problem.colors

elimCorners :: Int -> (Int, Int) -> BoardOf Attempt -> BoardOf Attempt
elimCorners size (x, y) board = foldr (.) id elimFns $ board
    where
    elimFns = [ insertIfAbsent (x', y') Eliminated | x' <- [x - 1, x + 1], y' <- [y - 1, y + 1], x' >= 0, x' < size, y' >= 0, y' < size ]

elimColumn :: Int -> Int -> BoardOf Attempt -> BoardOf Attempt
elimColumn size y board = foldr elimRow board [0 .. size - 1]
    where
    elimRow x' = insertIfAbsent (x', y) Eliminated

elimRow :: Int -> Int -> BoardOf Attempt -> BoardOf Attempt
elimRow size x board = foldr elimColumn board [0 .. size - 1]
    where
    elimColumn y' = insertIfAbsent (x, y') Eliminated

elimColor :: Problem -> Color -> BoardOf Attempt -> BoardOf Attempt
elimColor problem color = foldr (.) id elimFns
    where
        elimFns = [ insertIfAbsent (x, y) Eliminated | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1], Map.lookup (x, y) problem.colors == Just color ]

openCells :: (MonadLogic m) => Int -> BoardOf Attempt -> m (Int, Int)
openCells size board = foldr interleave empty [ pure (x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1], Map.lookup (x, y) board == Nothing ]

solve :: (MonadLogic m) => Problem -> BoardOf Attempt -> m (BoardOf Attempt)
solve problem board = do
    ifte 
        (openCells problem.size board)
        (\ (x, y) -> do
            let newBoard = placeQueen problem (x, y) board
            solve problem newBoard)
        (pure board)

solutions :: (MonadLogic m) => Problem -> m (BoardOf Attempt)
solutions problem = do 
    candidate <- solve problem (Map.empty)
    let queens = queenView candidate
    -- Completeness: ensure enough queens were placed
    -- guard (length queens == problem.size)
    pure candidate


mkProblem :: [[Color]] -> Problem
mkProblem colorsList = Problem {
    colors = Map.fromList [((i, j), color) | (i, row) <- zip [0..] colorsList, (j, color) <- zip [0..] row],
    size = length colorsList
}

exampleProblem :: Problem
exampleProblem = mkProblem
    [ [0, 1, 1, 1, 1]
    , [2, 3, 2, 1, 2]
    , [2, 3, 2, 1, 2]
    , [2, 2, 2, 4, 4]
    , [2, 2, 2, 2, 2]
    ]

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


queenView :: BoardOf Attempt -> [(Int, Int)]
queenView board = [(x, y) | ((x, y), HasQueen) <- Map.toList board]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
