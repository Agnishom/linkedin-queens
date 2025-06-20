module Main where

import Attempt5
import Control.Monad.Logic
import Problem

main :: IO ()
main = do
  let solution1 = observe $ solution problem1
  putStrLn "Solution for problem:"
  print solution1
  let solution2 = observe $ solution problem2
  putStrLn "Solution for problem2:"
  print solution2
  let solution3 = observe $ solution problem3
  putStrLn "Solution for problem3:"
  print solution3
  let solution4 = observe $ solution problem4
  putStrLn "Solution for problem4:"
  print solution4
  let solution5 = observe $ solution problem5
  putStrLn "Solution for problem5:"
  print solution5
