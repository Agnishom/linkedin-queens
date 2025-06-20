module Attempt4
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Problem

data Attempt = HasQueen | Eliminated
  deriving (Show, Eq)

data Remaining a = Satisfied | AvailableCandidates (Set a)
  deriving (Show, Eq)

remove :: (MonadLogic m, Ord k, Ord a) => k -> a -> Map k (Remaining a) -> m (Map k (Remaining a))
remove key a candidates = do
  case Map.lookup key candidates of
    Just (AvailableCandidates s) -> do
      -- Note: the condition is (|s| > 1)
      -- we do not want to be left with 0 candidates
      guard (Set.size s > 1)
      let newSet = Set.delete a s
      pure $ Map.insert key (AvailableCandidates newSet) candidates
    _ -> pure candidates

-- | Data structure representing our progress in solving the problem
data Partial = Partial
  { -- | Whether each square has a queen, or has been eliminated
    attempts :: Map (Row, Column) Attempt,
    -- | For each row i, whether it already has a queen,
    -- | or a set of columns j, such that placing a queen
    -- | in (i, j) is still possible
    rowCandidates :: Map Row (Remaining Column),
    -- | Candidates (i, j) for each column j, or
    -- | whether the column already has a queen
    columnCandidates :: Map Column (Remaining Row),
    -- | Candidates (i, j) for each color, or
    -- | whether the colored region already has a queen
    colorCandidates :: Map Color (Remaining (Row, Column))
  }
  deriving (Show, Eq)

-- | Mark a cell in the board as eliminated, and update the candidates accordingly
-- | If the cell already has a queen, do nothing
eliminate :: (MonadLogic m) => (Row, Column) -> Problem -> Partial -> m Partial
eliminate (x, y) problem partial
  | isJust currentCellValue = pure partial
  | otherwise = do
      let newAttempts = Map.insert (x, y) Eliminated partial.attempts
      newRowCandidates <- remove x y partial.rowCandidates
      newColumnCandidates <- remove y x partial.columnCandidates
      newColorCandidates <- remove color (x, y) partial.colorCandidates
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

-- | Place a queen in the given cell, and update the partial progress
-- | This eiliminates a number of other candidates sharing the same
-- | row, column, or color, and those which are in the corners of the cell
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

-- | A strategy is a set of (Row, Column) candidates such that at least one
-- | of them must be included in the completion of the solution
type Strategy = Set (Row, Column)

strategies :: Partial -> [Strategy]
strategies partial =
  [Set.fromList [(r, c) | c <- Set.toList s] | (r, AvailableCandidates s) <- Map.toList partial.rowCandidates]
    ++ [Set.fromList [(r, c) | r <- Set.toList s] | (c, AvailableCandidates s) <- Map.toList partial.columnCandidates]
    ++ [Set.fromList [(i, j) | (i, j) <- Set.toList s] | (_, AvailableCandidates s) <- Map.toList partial.colorCandidates]

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Partial -> m (Row, Column)
candidate partial
  | null availableStrategies = empty
  | otherwise = choose bestStrategy
  where
    availableStrategies = strategies partial
    -- We choose the strategy with the least number of candidates
    bestStrategy = minimumBy (compare `on` Set.size) availableStrategies

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  -- if there are no candidates left, we need to abort this branch
  ifte -- if-then-else
    (candidate partial) -- choose a candidate
    ( \(x, y) -> do
        -- place the queen in the chosen cell
        newPartial <- placeQueen problem (x, y) partial
        -- continue to place the rest of the queens
        solve problem newPartial
    )
    -- if there are no more candidates, return the current state
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

-- -- | Create an initial empty partial solution for the given problem
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