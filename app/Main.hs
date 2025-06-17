module Main where

import Control.Monad.Logic

import Problem
import Attempt4

main :: IO ()
main = do
  let solution1 = observe $ solutions exampleProblem
  putStrLn "Solution for exampleProblem:"
  print solution1
  let solution2 = observe $ solutions exampleProblem2
  putStrLn "Solution for exampleProblem2:"
  print solution2
  let solution3 = observe $ solutions exampleProblem3
  putStrLn "Solution for exampleProblem3:"
  print solution3
  let solution4 = observe $ solutions exampleProblem4
  putStrLn "Solution for exampleProblem4:"
  print solution4
  let solution5 = observe $ solutions exampleProblem5
  putStrLn "Solution for exampleProblem5:"
  print solution5
