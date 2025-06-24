module Attempt2
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Problem

data Attempt = HasQueen | Eliminated
  deriving (Show, Eq)

data Remaining = Satisfied | AvailableCandidates Int
  deriving (Show, Eq)

data Partial = Partial
  { attempts :: Map (Row, Column) Attempt,
    rowCandidates :: Map Row Remaining,
    columnCandidates :: Map Column Remaining,
    colorCandidates :: Map Color Remaining
  }
  deriving (Show, Eq)

decrease :: (MonadLogic m, Ord k) => k -> Map k Remaining -> m (Map k Remaining)
decrease key candidates =
  case Map.lookup key candidates of
    Just (AvailableCandidates n) -> do
      -- Note: the condition is (n > 1)
      -- we do not want to be left with 0 candidates
      guard (n > 1)
      pure $ Map.insert key (AvailableCandidates (n - 1)) candidates
    _ -> pure candidates

eliminate :: (MonadLogic m) => (Row, Column) -> Problem -> Partial -> m Partial
eliminate (x, y) problem partial
  | isJust currentCellValue = pure partial
  | otherwise = do
      let newAttempts = Map.insert (x, y) Eliminated partial.attempts
      newRowCandidates <- decrease x partial.rowCandidates
      newColumnCandidates <- decrease y partial.columnCandidates
      newColorCandidates <- decrease color partial.colorCandidates
      pure $
        Partial
          { attempts = newAttempts,
            rowCandidates = newRowCandidates,
            columnCandidates = newColumnCandidates,
            colorCandidates = newColorCandidates
          }
  where
    color = problem ! (x, y)
    currentCellValue = Map.lookup (x, y) partial.attempts

elimCorners :: (MonadLogic m) => (Row, Column) -> Problem -> Partial -> m Partial
elimCorners (x, y) problem = foldr (>=>) pure elimFns
  where
    elimFns =
      [ eliminate (x', y') problem
        | x' <- [x - 1, x + 1],
          x' >= 0,
          x' < size problem,
          y' <- [y - 1, y + 1],
          y' >= 0,
          y' < size problem
      ]

elimColumn :: (MonadLogic m) => Column -> Problem -> Partial -> m Partial
elimColumn j problem partial = foldM (flip elimCell) partial [0 .. size problem - 1]
  where
    elimCell i = eliminate (i, j) problem

elimRow :: (MonadLogic m) => Row -> Problem -> Partial -> m Partial
elimRow i problem partial = foldM (flip elimCell) partial [0 .. size problem - 1]
  where
    elimCell j = eliminate (i, j) problem

elimColor :: (MonadLogic m) => Color -> Problem -> Partial -> m Partial
elimColor color problem = foldr (>=>) pure elimFns
  where
    elimFns = [eliminate (x, y) problem | x <- [0 .. size problem - 1], y <- [0 .. size problem - 1], problem ! (x, y) == color]

placeQueen :: (MonadLogic m) => Problem -> (Row, Column) -> Partial -> m Partial
placeQueen problem (x, y) partial = elimAll newPartial
  where
    newAttempts = Map.insert (x, y) HasQueen partial.attempts
    newRowCandidates = Map.insert x Satisfied partial.rowCandidates
    newColumnCandidates = Map.insert y Satisfied partial.columnCandidates
    color = problem ! (x, y)
    newColorCandidates = Map.insert color Satisfied partial.colorCandidates
    newPartial =
      Partial
        { attempts = newAttempts,
          rowCandidates = newRowCandidates,
          columnCandidates = newColumnCandidates,
          colorCandidates = newColorCandidates
        }
    elimAll =
      elimCorners (x, y) problem
        >=> elimColumn y problem
        >=> elimRow x problem
        >=> elimColor color problem

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
candidate problem partial = do
  let n = size problem
  x <- choose [0 .. n - 1]
  y <- choose [0 .. n - 1]
  guard (isNothing (Map.lookup (x, y) partial.attempts))
  pure (x, y)

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  ifte
    (candidate problem partial)
    ( \pos -> do
        newPartial <- placeQueen problem pos partial
        solve problem newPartial
    )
    (pure partial)

queenView :: Partial -> [(Row, Column)]
queenView partial = [(x, y) | ((x, y), status) <- Map.toList partial.attempts, status == HasQueen]

solution :: (MonadLogic m) => Problem -> m [(Row, Column)]
solution problem = do
  endState <- solve problem (mkPartial problem)
  let queens = queenView endState
  -- Completeness: ensure enough queens were placed
  guard (length queens == size problem)
  pure queens

mkPartial :: Problem -> Partial
mkPartial problem =
  Partial
    { attempts = Map.empty,
      rowCandidates = Map.fromList [(r, AvailableCandidates (size problem)) | r <- [0 .. size problem - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates (size problem)) | c <- [0 .. size problem - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates $ colorSize color) | color <- [0 .. size problem - 1]]
    }
  where
    colorSize color = length [(i, j) | i <- [0 .. size problem - 1], j <- [0 .. size problem - 1], problem ! (i, j) == color]