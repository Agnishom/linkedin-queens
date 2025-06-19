module Problem
  ( Color,
    Row,
    Column,
    Problem,
    mkProblem,
    (!),
    size,
    problem1,
    problem2,
    problem3,
    problem4,
    problem5,
  )
where

import Data.Array (Array, (!))
import qualified Data.Array as Array

type Color = Int

type Row = Int

type Column = Int

type Problem = Array (Row, Column) Color

mkProblem :: [[Color]] -> Problem
mkProblem colorsList =
  Array.array
    ((0, 0), (n - 1, n - 1))
    [((i, j), color) | (i, row) <- zip [0 ..] colorsList, (j, color) <- zip [0 ..] row]
  where
    n = length colorsList

size :: Problem -> Int
size problem = n + 1
  where
    ((_, _), (_, n)) = Array.bounds problem

-- Sample problem on https://www.linkedin.com/games/queens
problem1 :: Problem
problem1 =
  mkProblem
    [ [0, 1, 1, 1, 1],
      [2, 3, 2, 1, 2],
      [2, 3, 2, 1, 2],
      [2, 2, 2, 4, 4],
      [2, 2, 2, 2, 2]
    ]

-- https://queensgame.vercel.app/level/1
problem2 :: Problem
problem2 =
  mkProblem
    [ [0, 0, 1, 1, 1, 2, 2, 2],
      [0, 3, 1, 3, 1, 4, 2, 2],
      [0, 3, 1, 3, 1, 2, 2, 2],
      [0, 3, 3, 3, 1, 5, 6, 2],
      [0, 3, 3, 3, 1, 5, 6, 6],
      [0, 3, 7, 3, 1, 5, 6, 6],
      [7, 3, 7, 3, 1, 5, 5, 6],
      [7, 7, 7, 7, 6, 6, 6, 6]
    ]

-- https://queensgame.vercel.app/community-level/1
problem3 :: Problem
problem3 =
  mkProblem
    [ [0, 0, 0, 1, 2, 3],
      [0, 0, 0, 1, 2, 3],
      [1, 1, 1, 1, 2, 3],
      [1, 1, 4, 2, 2, 3],
      [5, 4, 4, 2, 2, 3],
      [5, 5, 4, 4, 2, 2]
    ]

-- https://queensgame.vercel.app/community-level/3
problem4 :: Problem
problem4 =
  mkProblem
    [ [0, 0, 0, 1, 1, 1, 1],
      [0, 0, 1, 1, 1, 1, 1],
      [2, 2, 2, 1, 3, 3, 3],
      [2, 4, 4, 4, 3, 3, 3],
      [2, 4, 4, 4, 3, 3, 3],
      [2, 4, 4, 4, 5, 5, 6],
      [2, 2, 2, 2, 5, 5, 5]
    ]

problem5 :: Problem
problem5 =
  mkProblem
    [ [0, 1, 1, 1, 2, 1, 1, 3],
      [0, 0, 0, 1, 2, 2, 1, 3],
      [1, 1, 1, 1, 1, 2, 1, 3],
      [4, 4, 4, 1, 1, 1, 1, 3],
      [4, 1, 1, 1, 1, 1, 1, 1],
      [1, 1, 1, 1, 5, 1, 1, 6],
      [7, 7, 1, 5, 5, 1, 6, 6],
      [7, 7, 1, 5, 1, 1, 1, 6]
    ]