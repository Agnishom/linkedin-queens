module Main (main) where

import qualified Attempt0
import qualified Attempt1
import qualified Attempt2
import qualified Attempt3
import qualified Attempt4
import qualified Attempt5Heap
import qualified Attempt5Set
import qualified Attempt6
import Control.Monad.Logic
import Paths_linkedin_queens
import Problem
import qualified SMT
import System.Directory (listDirectory)
import Test.Tasty.Bench

-- List of problem names and values
problems :: [(String, Problem)]
problems =
  [ ("problem1", problem1),
    ("problem2", problem2),
    ("problem3", problem3),
    ("problem4", problem4),
    ("problem5", problem5)
  ]

type Solution = [(Row, Column)]

type Solver m = Problem -> m Solution

-- List of solver names and their solution functions
solvers :: (MonadLogic m) => [(String, Solver m)]
solvers =
  [ ("Attempt1", Attempt1.solution),
    ("Attempt2", Attempt2.solution),
    ("Attempt3", Attempt3.solution),
    ("Attempt4", Attempt4.solution),
    ("Attempt5Heap", Attempt5Heap.solution),
    ("Attempt5Set", Attempt5Set.solution),
    ("Attempt6", Attempt6.solution)
  ]

-- Generate benchmarks for each (solver, problem) pair
main :: IO ()
main = do
  defaultMain $
    -- Static in-memory benchmarks
    [ bgroup
        problemName
        [ bench solverName $
            nf (observe . solver) problem
          | (solverName, solver) <- ("Attempt0", Attempt0.solution) : solvers
        ]
      | (problemName, problem) <- problems
    ]
      ++
      -- One bgroup for all disk benchmarks
      -- by inspection, we know that the problems are of size 7, 8, 9, 10 and 11
      [ bgroup "disk-problems" $ diskBenchmarks [7, 8, 9, 10, 11]
      ]
      ++
      -- One bgroup for all SMT benchmarks
      [ bgroup "SMT" (smtBenchmarks [7, 8, 9, 10, 11])
      ]
      ++
      [ bench "problem1-smt" $ nfIO (SMT.solution problem1),
        bench "problem2-smt" $ nfIO (SMT.solution problem2),
        bench "problem3-smt" $ nfIO (SMT.solution problem3),
        bench "problem4-smt" $ nfIO (SMT.solution problem4),
        bench "problem5-smt" $ nfIO (SMT.solution problem5)
      ]

getProblems :: IO [Problem]
getProblems = do
  dd <- getDataDir
  let levelsDir = dd <> "/levels"
  fileNames <- listDirectory levelsDir
  fileContents <- mapM (\fileName -> readFile (levelsDir <> "/" <> fileName)) fileNames
  pure $ map parse fileContents

getProblemsBySize :: Int -> IO [Problem]
getProblemsBySize n = filter (\p -> size p == n) <$> getProblems

-- solves the problems of size n
solveProblemsOfSize :: Int -> Solver Logic -> IO [Solution]
solveProblemsOfSize n solver = do
  map (observe . solver) <$> getProblemsBySize n

diskBenchmarks :: [Int] -> [Benchmark]
diskBenchmarks = concatMap perSizeBenchmarks
  where
    perSizeBenchmarks n =
      map
        ( \(solverName, solver) ->
            bench (solverName ++ "-size-" ++ show n) $
              nfIO (solveProblemsOfSize n solver)
        )
        solvers

smtSolve :: Int -> IO [Solution]
smtSolve n =
  getProblemsBySize n
    >>= mapM SMT.solution

smtBenchmarks :: [Int] -> [Benchmark]
smtBenchmarks = concatMap perSizeBenchmarks
  where
    perSizeBenchmarks n =
      [ bench ("SMT-size-" ++ show n) $
          nfIO (smtSolve n)
      ]