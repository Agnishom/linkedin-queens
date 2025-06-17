module Problem (
    Color,
    Row,
    Column,
    BoardOf,
    Problem(..),
    exampleProblem,
    exampleProblem2,
    exampleProblem3,
    exampleProblem4,
    exampleProblem5,
)
where

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

mkProblem :: [[Color]] -> Problem
mkProblem colorsList = Problem {
    colors = Map.fromList [((i, j), color) | (i, row) <- zip [0..] colorsList, (j, color) <- zip [0..] row],
    size = length colorsList
}

-- Sample problem on https://www.linkedin.com/games/queens
exampleProblem :: Problem
exampleProblem = mkProblem
    [ [0, 1, 1, 1, 1]
    , [2, 3, 2, 1, 2]
    , [2, 3, 2, 1, 2]
    , [2, 2, 2, 4, 4]
    , [2, 2, 2, 2, 2]
    ]

-- https://queensgame.vercel.app/level/1
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