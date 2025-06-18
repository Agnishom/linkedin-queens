module Attempt2
  ( solutions,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Map (Map)
import qualified Data.Map as Map
import Problem

data Attempt = HasQueen | Eliminated
  deriving (Show, Eq)

data Remaining = Satisfied | AvailableCandidates Int
  deriving (Show, Eq)

decrease :: Remaining -> Remaining
decrease (AvailableCandidates n)
  | n > 0 = AvailableCandidates (n - 1)
  | otherwise = error "Cannot decrease Remaining below 0"
decrease Satisfied = Satisfied

data Partial = Partial
  { attempts :: BoardOf Attempt,
    rowCandidates :: Map Row Remaining,
    columnCandidates :: Map Column Remaining,
    colorCandidates :: Map Color Remaining
  }
  deriving (Show, Eq)

eliminate :: (Row, Column) -> Problem -> Partial -> Partial
eliminate (x, y) problem partial =
  Partial
    { attempts = newAttempts,
      rowCandidates = newRowCandidates,
      columnCandidates = newColumnCandidates,
      colorCandidates = newColorCandidates
    }
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
elimCorners (x, y) problem = foldr (.) id elimFns
  where
    elimFns =
      [ eliminate (x', y') problem
        | x' <- [x - 1, x + 1],
          x' >= 0,
          x' < problem.size,
          y' <- [y - 1, y + 1],
          y' >= 0,
          y' < problem.size
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
    elimFns = [eliminate (x, y) problem | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1], Map.lookup (x, y) problem.colors == Just color]

placeQueen :: Problem -> (Row, Column) -> Partial -> Partial
placeQueen problem (x, y) partial =
  elimCorners (x, y) problem
    . elimColumn y problem
    . elimRow x problem
    . maybe id (`elimColor` problem) cellColor
    $ Partial
      { attempts = newAttempts,
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

genCandidates :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
genCandidates problem partial = foldr interleave empty [pure (x, y) | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1], Map.lookup (x, y) partial.attempts == Nothing]

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
  guard (not (outOfCandidates partial))
  ifte
    (genCandidates problem partial)
    ( \(x, y) -> do
        let newPartial = placeQueen problem (x, y) partial
        solve problem newPartial
    )
    (pure partial)

queenView :: Partial -> [(Row, Column)]
queenView partial = [(x, y) | ((x, y), HasQueen) <- Map.toList partial.attempts]

solutions :: (MonadLogic m) => Problem -> m [(Row, Column)]
solutions problem = do
  candidate <- solve problem (mkPartial problem)
  let queens = queenView candidate
  -- Completeness: ensure enough queens were placed
  guard (length queens == problem.size)
  pure queens

mkPartial :: Problem -> Partial
mkPartial problem =
  Partial
    { attempts = Map.empty,
      rowCandidates = Map.fromList [(r, AvailableCandidates problem.size) | r <- [0 .. problem.size - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates problem.size) | c <- [0 .. problem.size - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates problem.size) | color <- [0 .. problem.size - 1]]
    }