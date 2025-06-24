module Attempt5Heap
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Set (Set)
import qualified Data.Set as Set
import Problem

newtype Strategy = Strategy {unStrategy :: Set (Row, Column)}
  deriving (Show, Eq)

instance Ord Strategy where
  compare (Strategy s1) (Strategy s2) =
    -- prefer smaller sets first
    case compare (Set.size s1) (Set.size s2) of
      EQ -> compare s1 s2
      other -> other

data Partial = Partial
  { queens :: Set (Row, Column),
    strategies :: Heap Strategy
  }
  deriving (Show, Eq)

directlyAttacked :: (Row, Column) -> Problem -> Set (Row, Column)
directlyAttacked (x, y) problem = Set.fromList $ corners ++ row ++ column ++ color
  where
    n = size problem
    corners =
      [ (x', y')
        | x' <- [x - 1, x + 1],
          x' >= 0,
          x' < n,
          y' <- [y - 1, y + 1],
          y' >= 0,
          y' < n
      ]
    row = [(x, j) | j <- [0 .. n - 1], j /= y]
    column = [(i, y) | i <- [0 .. n - 1], i /= x]
    color = [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], problem ! (i, j) == problem ! (x, y), (i, j) /= (x, y)]

eliminateFromStrategy :: (MonadLogic m) => Set (Row, Column) -> Strategy -> m Strategy
eliminateFromStrategy attacked (Strategy s) = do
  let newSet = Set.difference s attacked
  if Set.null newSet
    then empty -- fail if the strategy is empty
    else pure $ Strategy newSet

placeQueen :: (MonadLogic m) => Problem -> (Row, Column) -> Partial -> m Partial
placeQueen problem (x, y) partial = do
  -- add a queen to the board
  let newQueens = Set.insert (x, y) partial.queens
  -- remove all the strategies which contains the new queen
  let strat1 = Heap.filter (Set.notMember (x, y) . (.unStrategy)) partial.strategies
  -- from each strategy, remove the directly attacked candidates
  -- fail if this makes the strategy empty
  let attacked = directlyAttacked (x, y) problem
  strat2 <- Heap.mapM (eliminateFromStrategy attacked) strat1
  -- eliminate duplicates in the strategies
  let newStrategies = Heap.nub strat2
  pure $
    Partial
      { queens = newQueens,
        strategies = newStrategies
      }

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Partial -> m (Row, Column)
candidate partial = do
  -- if there are no strategies left, we cannot proceed
  guard $ not (Heap.null partial.strategies)
  -- choose the strategy with the smallest set of candidates
  let Strategy s = Heap.minimum partial.strategies
  -- choose a queen from the strategy
  choose s

-- do an action m times
repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM n f = foldr (>=>) pure (replicate n f)

extend :: (MonadLogic m) => Problem -> Partial -> m Partial
extend problem partial = do
  pos <- candidate partial
  -- place a queen on the candidate cell
  placeQueen problem pos partial

solution :: (MonadLogic m) => Problem -> m [(Row, Column)]
solution problem = do
  endState <- repeatM (size problem) (extend problem) (mkPartial problem)
  pure $ Set.toList endState.queens

-- | Create an initial empty partial solution for the given problem
mkPartial :: Problem -> Partial
mkPartial problem =
  Partial
    { queens = Set.empty,
      strategies = Heap.fromList $ map (Strategy . Set.fromList) (rowStrategies ++ columnStrategies ++ colorStrategies)
    }
  where
    n = size problem
    rowStrategies = [[(r, c) | c <- [0 .. n - 1]] | r <- [0 .. n - 1]]
    columnStrategies = [[(r, c) | r <- [0 .. n - 1]] | c <- [0 .. n - 1]]
    colorStrategies = [[(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], problem ! (i, j) == color] | color <- [0 .. n - 1]]