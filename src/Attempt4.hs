module Attempt4
  ( solutions,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.List (minimumBy)
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

-- | Data structure representing our progress in solving the problem
data Partial = Partial
  { -- | Whether each square has a queen, or has been eliminated
    attempts :: BoardOf Attempt,
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

-- | Place a queen in the given cell, and update the partial progress
-- | This eiliminates a number of other candidates sharing the same
-- | row, column, or color, and those which are in the corners of the cell
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

-- | A strategy is a set of (Row, Column) candidates such that at least one
-- | of them must be included in the completion of the solution
data Strategy
  = RowStrategy Row (Set Column)
  | ColumnStrategy Column (Set Row)
  | ColorStrategy Color (Set (Row, Column))

-- | A strategy is better than another if it has fewer candidates
compareStrategies :: Strategy -> Strategy -> Ordering
compareStrategies cand1 cand2 = compare (size cand1) (size cand2)
  where
    size (RowStrategy _ s) = Set.size s
    size (ColumnStrategy _ s) = Set.size s
    size (ColorStrategy _ s) = Set.size s

allStrategies :: Partial -> [Strategy]
allStrategies partial =
  [RowStrategy r s | (r, AvailableCandidates s) <- Map.toList partial.rowCandidates]
    ++ [ColumnStrategy c s | (c, AvailableCandidates s) <- Map.toList partial.columnCandidates]
    ++ [ColorStrategy color s | (color, AvailableCandidates s) <- Map.toList partial.colorCandidates]

expandStrategies :: Problem -> Partial -> Strategy -> [(Row, Column)]
expandStrategies problem partial (RowStrategy r s) = [(r, c) | c <- Set.toList s, checkRowCandidate (r, c) problem partial]
expandStrategies problem partial (ColumnStrategy c s) = [(r, c) | r <- Set.toList s, checkColumnCandidate (r, c) problem partial]
expandStrategies _ partial (ColorStrategy _ s) = [(i, j) | (i, j) <- Set.toList s, checkColorCandidate (i, j) partial]

checkRowCandidate :: (Row, Column) -> Problem -> Partial -> Bool
checkRowCandidate (r, c) problem partial = columnCheck && colorCheck
  where
    columnCheck = case Map.lookup c partial.columnCandidates of
      Just (AvailableCandidates s) -> Set.member r s
      _ -> False
    cellColor = problem.colors ! (r, c)
    colorCheck = case Map.lookup cellColor partial.colorCandidates of
      Just (AvailableCandidates s) -> Set.member (r, c) s
      _ -> False

checkColumnCandidate :: (Row, Column) -> Problem -> Partial -> Bool
checkColumnCandidate (r, c) problem partial = rowCheck && colorCheck
  where
    rowCheck = case Map.lookup r partial.rowCandidates of
      Just (AvailableCandidates s) -> Set.member c s
      _ -> False
    cellColor = problem.colors ! (r, c)
    colorCheck = case Map.lookup cellColor partial.colorCandidates of
      Just (AvailableCandidates s) -> Set.member (r, c) s
      _ -> False

checkColorCandidate :: (Row, Column) -> Partial -> Bool
checkColorCandidate (r, c) partial = rowCheck && columnCheck
  where
    rowCheck = case Map.lookup r partial.rowCandidates of
      Just (AvailableCandidates s) -> Set.member c s
      _ -> False
    columnCheck = case Map.lookup c partial.columnCandidates of
      Just (AvailableCandidates s) -> Set.member r s
      _ -> False

genCandidates :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
genCandidates problem partial
  | null strategies = empty
  | otherwise = foldr interleave empty $ [pure (x, y) | (x, y) <- expandStrategies problem partial bestStrategy]
  where
    strategies = allStrategies partial
    -- We choose the strategy with the least number of candidates
    bestStrategy = minimumBy compareStrategies strategies

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  -- if there are no candidates left, we need to abort this branch
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

-- | Create an initial empty partial solution for the given problem
mkPartial :: Problem -> Partial
mkPartial problem =
  Partial
    { attempts = Map.empty,
      rowCandidates = Map.fromList [(r, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | r <- [0 .. problem.size - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | c <- [0 .. problem.size - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates (Set.fromList [(x, y) | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1]])) | color <- [0 .. problem.size - 1]]
    }