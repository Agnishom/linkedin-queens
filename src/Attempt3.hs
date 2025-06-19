module Attempt3
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Problem

data Attempt = HasQueen | Eliminated
  deriving (Show, Eq)

type Board = Map (Row, Column)

data Remaining a = Satisfied | AvailableCandidates (Set a)
  deriving (Show, Eq)

remove :: (Ord a) => a -> Remaining a -> Remaining a
remove x (AvailableCandidates s)
  | Set.size s > 0 = AvailableCandidates (Set.delete x s)
  | otherwise = error "Cannot remove from an empty set"
remove _ Satisfied = Satisfied

data Partial = Partial
  { attempts :: Board Attempt,
    rowCandidates :: Map Row (Remaining Column),
    columnCandidates :: Map Column (Remaining Row),
    colorCandidates :: Map Color (Remaining (Row, Column))
  }
  deriving (Show, Eq)

eliminate :: (Row, Column) -> Problem -> Partial -> Partial
eliminate (x, y) problem partial
  | isJust currentCellValue = partial
  | otherwise =
      Partial
        { attempts = newAttempts,
          rowCandidates = newRowCandidates,
          columnCandidates = newColumnCandidates,
          colorCandidates = newColorCandidates
        }
  where
    color = problem ! (x, y)
    currentCellValue = Map.lookup (x, y) partial.attempts
    newAttempts = Map.insert (x, y) Eliminated partial.attempts
    newRowCandidates = Map.adjust (remove y) x partial.rowCandidates
    newColumnCandidates = Map.adjust (remove x) y partial.columnCandidates
    newColorCandidates = Map.adjust (remove (x, y)) color partial.colorCandidates

elimCorners :: (Row, Column) -> Problem -> Partial -> Partial
elimCorners (x, y) problem = foldr (.) id elimFns
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

elimColumn :: Column -> Problem -> Partial -> Partial
elimColumn j problem partial = foldr elimCell partial [0 .. size problem - 1]
  where
    elimCell i = eliminate (i, j) problem

elimRow :: Row -> Problem -> Partial -> Partial
elimRow i problem partial = foldr elimCell partial [0 .. size problem - 1]
  where
    elimCell j = eliminate (i, j) problem

elimColor :: Color -> Problem -> Partial -> Partial
elimColor color problem = foldr (.) id elimFns
  where
    elimFns = [eliminate (x, y) problem | x <- [0 .. size problem - 1], y <- [0 .. size problem - 1], problem ! (x, y) == color]

placeQueen :: Problem -> (Row, Column) -> Partial -> Partial
placeQueen problem (x, y) partial =
  elimCorners (x, y) problem
    . elimColumn y problem
    . elimRow x problem
    . elimColor color problem
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
    newColorCandidates = Map.insert color Satisfied partial.colorCandidates
    color = problem ! (x, y)

outOfCandidates :: Partial -> Bool
outOfCandidates partial = outOfRowCandidates || outOfColumnCandidates || outOfColorCandidates
  where
    outOfRowCandidates = any isOut (Map.elems partial.rowCandidates)
    outOfColumnCandidates = any isOut (Map.elems partial.columnCandidates)
    outOfColorCandidates = any isOut (Map.elems partial.colorCandidates)
    isOut (AvailableCandidates s) = Set.size s == 0
    isOut _ = False

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

rowCandidate :: (MonadLogic m) => Partial -> m (Row, Column)
rowCandidate partial = do
  (r, remaining) <- choose $ Map.toList partial.rowCandidates
  s <- case remaining of
    AvailableCandidates s' -> pure s'
    Satisfied -> empty
  c <- choose s
  pure (r, c)

_columnCandidate :: (MonadLogic m) => Partial -> m (Row, Column)
_columnCandidate partial = do
  (c, remaining) <- choose $ Map.toList partial.columnCandidates
  s <- case remaining of
    AvailableCandidates s' -> pure s'
    Satisfied -> empty
  r <- choose s
  pure (r, c)

_colorCandidates :: (MonadLogic m) => Partial -> m (Row, Column)
_colorCandidates partial = do
  (_, remaining) <- choose $ Map.toList partial.colorCandidates
  s <- case remaining of
    AvailableCandidates s' -> pure s'
    Satisfied -> empty
  (i, j) <- choose s
  pure (i, j)

candidate :: (MonadLogic m) => Partial -> m (Row, Column)
candidate = rowCandidate

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  guard (not (outOfCandidates partial))
  ifte
    (candidate partial)
    ( \(x, y) -> do
        let newPartial = placeQueen problem (x, y) partial
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
      rowCandidates = Map.fromList [(r, AvailableCandidates (Set.fromList [0 .. size problem - 1])) | r <- [0 .. size problem - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates (Set.fromList [0 .. size problem - 1])) | c <- [0 .. size problem - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates (colorCandidates color)) | color <- [0 .. size problem - 1]]
    }
  where
    colorCandidates color = Set.fromList [(i, j) | i <- [0 .. size problem - 1], j <- [0 .. size problem - 1], problem ! (i, j) == color]