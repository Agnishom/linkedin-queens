module SMT
  ( solution,
  )
where

import Data.SBV.Trans
import Data.SBV.Trans.Control
import Problem

-- for each row, the column where the queen is placed
-- ranges from 0 to n-1
rangeConstraints :: [SInteger] -> SBool
rangeConstraints js = sAll (\y -> y .>= 0 .&& y .< n) js
  where
    n = fromIntegral $ length js

-- no two queens are in the same column
columnConstraints :: [SInteger] -> SBool
columnConstraints js = 
  sAll 
    (\(a, b) -> a ./= b) 
    [(a, b) | (i, a) <- zip [(0 :: Int) ..] js, (j, b) <- zip [0 ..] js, i < j]

-- no two queens are touching corners
-- queen[i] - queen[i+1] != 1 or -1
cornerConstraints :: [SInteger] -> SBool
cornerConstraints js =
  sAll
    (\(a, b) -> a - b ./= 1 .&& a - b ./= -1)
    [(a, b) | (a, b) <- zip js (drop 1 js)]

-- no two queens are in the same color region
-- for each pair of cells (i, j) and (k, l),
-- color ! (i, j) == color ! (k, l) ==> not (queen[i] = j && queen[k] = l)
colorConstraints :: Problem -> [SInteger] -> SBool
colorConstraints problem queens = 
    sAll 
      f 
      [(i, j, k, l) | 
        i <- [0 .. n - 1], 
        j <- [0 .. n - 1], 
        k <- [i .. n - 1], 
        l <- [0 .. n - 1], 
        (i /= k || j < l)]
  where
    n = size problem
    f (i, j, k, l) = 
      (fromBool $ problem ! (i, j) == problem ! (k, l)) 
        .=> sNot (queens !! i .== fromIntegral j .&& queens !! k .== fromIntegral l)

solution :: (ExtractIO m) => Problem -> m [(Row, Column)]
solution problem = runSMT $ do
  let n = size problem
  queens <- sIntegers . map (\i -> "q_" <> show i) $ [0 .. n - 1]
  constrain $ rangeConstraints queens
  constrain $ columnConstraints queens
  constrain $ cornerConstraints queens
  constrain $ colorConstraints problem queens
  query $ do
    result <- checkSat
    case result of
      Sat -> do
        queensValues <- mapM getValue queens
        pure [(i, fromIntegral j) | (i, j) <- zip [0 .. n - 1] queensValues]
      _ -> error "Puzzle has no solution"
