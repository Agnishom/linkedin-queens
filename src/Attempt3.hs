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

genRowCandidates :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
genRowCandidates problem partial = foldr interleave empty $ [gen r s | (r, s) <- Map.toList partial.rowCandidates, s /= Satisfied]
  where
    gen r (AvailableCandidates s) = foldr interleave empty [pure (r, c) | c <- Set.toList s, checkRowCandidate (r, c) problem partial]
    gen _ Satisfied = error "impossible"

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

genColumnCandidates :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
genColumnCandidates problem partial = foldr interleave empty $ [gen c s | (c, s) <- Map.toList partial.columnCandidates, s /= Satisfied]
  where
    gen c (AvailableCandidates s) = foldr interleave empty [pure (r, c) | r <- Set.toList s, checkColumnCandidate (r, c) problem partial]
    gen _ Satisfied = error "impossible"

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

genColorCandidates :: (MonadLogic m) => Partial -> m (Row, Column)
genColorCandidates partial = foldr interleave empty $ [pure (i, j) | (_, AvailableCandidates s) <- Map.toList partial.colorCandidates, (i, j) <- Set.toList s, checkColorCandidate (i, j) partial]

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
genCandidates problem partial =
  genRowCandidates problem partial `interleave` genColumnCandidates problem partial `interleave` genColorCandidates partial

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

mkPartial :: Problem -> Partial
mkPartial problem =
  Partial
    { attempts = Map.empty,
      rowCandidates = Map.fromList [(r, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | r <- [0 .. problem.size - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates (Set.fromList [0 .. problem.size - 1])) | c <- [0 .. problem.size - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates (Set.fromList [(x, y) | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1]])) | color <- [0 .. problem.size - 1]]
    }