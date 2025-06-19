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

type Board = Map (Row, Column)

data Remaining = Satisfied | AvailableCandidates Int
  deriving (Show, Eq)

decrease :: Remaining -> Remaining
decrease (AvailableCandidates n)
  | n > 0 = AvailableCandidates (n - 1)
  | otherwise = error "Cannot decrease Remaining below 0"
decrease Satisfied = Satisfied

data Partial = Partial
  { attempts :: Board Attempt,
    rowCandidates :: Map Row Remaining,
    columnCandidates :: Map Column Remaining,
    colorCandidates :: Map Color Remaining
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
    newRowCandidates = Map.adjust decrease x partial.rowCandidates
    newColumnCandidates = Map.adjust decrease y partial.columnCandidates
    newColorCandidates = Map.adjust decrease color partial.colorCandidates

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

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
candidate problem partial = do
  let n = size problem
  x <- choose [0 .. n - 1]
  y <- choose [0 .. n - 1]
  guard (isNothing (Map.lookup (x, y) partial.attempts))
  pure (x, y)

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
  -- if we are out of candidates, we abort this branch
  guard (not $ outOfCandidates partial)
  ifte
    (candidate problem partial)
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
      rowCandidates = Map.fromList [(r, AvailableCandidates (size problem)) | r <- [0 .. size problem - 1]],
      columnCandidates = Map.fromList [(c, AvailableCandidates (size problem)) | c <- [0 .. size problem - 1]],
      colorCandidates = Map.fromList [(color, AvailableCandidates $ colorSize color) | color <- [0 .. size problem - 1]]
    }
  where
    colorSize color = length [(i, j) | i <- [0 .. size problem - 1], j <- [0 .. size problem - 1], problem ! (i, j) == color]