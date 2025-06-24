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

data Remaining a = Satisfied | AvailableCandidates (Set a)
  deriving (Show, Eq)

data Partial = Partial
  { attempts :: Map (Row, Column) Attempt,
    rowCandidates :: Map Row (Remaining Column),
    columnCandidates :: Map Column (Remaining Row),
    colorCandidates :: Map Color (Remaining (Row, Column))
  }
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

_colorCandidate :: (MonadLogic m) => Partial -> m (Row, Column)
_colorCandidate partial = do
  (_, remaining) <- choose $ Map.toList partial.colorCandidates
  s <- case remaining of
    AvailableCandidates s' -> pure s'
    Satisfied -> empty
  (i, j) <- choose s
  pure (i, j)

candidate :: (MonadLogic m) => Partial -> m (Row, Column)
candidate = rowCandidate

-- do an action n times
repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM n f = foldr (>=>) pure (replicate n f)

extend :: (MonadLogic m) => Problem -> Partial -> m Partial
extend problem partial = do
  pos <- candidate partial
  -- place a queen on the candidate cell
  placeQueen problem pos partial

queenView :: Partial -> [(Row, Column)]
queenView partial = [(x, y) | ((x, y), status) <- Map.toList partial.attempts, status == HasQueen]

solution :: (MonadLogic m) => Problem -> m [(Row, Column)]
solution problem = do
  endState <- repeatM (size problem) (extend problem) (mkPartial problem)
  pure $ queenView endState

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