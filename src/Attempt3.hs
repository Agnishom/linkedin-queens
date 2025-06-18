module Attempt3
  ( solutions,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Problem

data Attempt = HasQueen | Eliminated
  deriving (Show, Eq)

data Remaining a = Satisfied | AvailableCandidates (Set a)
  deriving (Show, Eq)

remove :: (Ord a) => a -> Remaining a -> Remaining a
remove x (AvailableCandidates s)
  | Set.size s > 0 = AvailableCandidates (Set.delete x s)
  | otherwise = error "Cannot remove from an empty set"
remove _ Satisfied = Satisfied

data Partial = Partial
  { attempts :: BoardOf Attempt,
    rowCandidates :: Map Row (Remaining Column),
    columnCandidates :: Map Column (Remaining Row),
    colorCandidates :: Map Color (Remaining (Row, Column))
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
      Nothing -> Map.adjust (remove y) x partial.rowCandidates
      Just _ -> partial.rowCandidates
    newColumnCandidates = case currentCellValue of
      Nothing -> Map.adjust (remove x) y partial.columnCandidates
      Just _ -> partial.columnCandidates
    newColorCandidates = case (currentCellValue, cellColor) of
      (Nothing, Just color) -> Map.adjust (remove (x, y)) color partial.colorCandidates
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

outOfCandidates :: Partial -> Bool
outOfCandidates partial = outOfRowCandidates || outOfColumnCandidates || outOfColorCandidates
  where
    outOfRowCandidates = any isOut (Map.elems partial.rowCandidates)
    outOfColumnCandidates = any isOut (Map.elems partial.columnCandidates)
    outOfColorCandidates = any isOut (Map.elems partial.colorCandidates)
    isOut (AvailableCandidates s) = Set.size s == 0
    isOut _ = False

genRowCandidates :: (MonadLogic m) => Partial -> m (Row, Column)
genRowCandidates partial = foldr interleave empty $ [gen r s | (r, AvailableCandidates s) <- Map.toList partial.rowCandidates]
  where
    gen r s = foldr interleave empty [pure (r, c) | c <- Set.toList s]

_genColumnCandidates :: (MonadLogic m) => Partial -> m (Row, Column)
_genColumnCandidates partial = foldr interleave empty $ [gen c s | (c, AvailableCandidates s) <- Map.toList partial.columnCandidates]
  where
    gen c s = foldr interleave empty [pure (r, c) | r <- Set.toList s]

_genColorCandidates :: (MonadLogic m) => Partial -> m (Row, Column)
_genColorCandidates partial = foldr interleave empty $ [pure (i, j) | (_, AvailableCandidates s) <- Map.toList partial.colorCandidates, (i, j) <- Set.toList s]

genCandidates :: (MonadLogic m) => Partial -> m (Row, Column)
genCandidates = genRowCandidates

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  -- if there are no candidates left, we need to abort this branch
  guard (not (outOfCandidates partial))
  ifte
    (genCandidates partial)
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
      rowCandidates = Map.fromList [(r, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | r <- [0 .. problem.size - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | c <- [0 .. problem.size - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates (colorCandidates color)) | color <- [0 .. problem.size - 1]]
    }
  where
    colorCandidates color = Set.fromList [(i, j) | i <- [0 .. problem.size - 1], j <- [0 .. problem.size - 1], problem.colors ! (i, j) == color]