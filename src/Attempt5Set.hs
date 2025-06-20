module Attempt5Set
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
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
    strategies :: Set Strategy
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

eliminateFromStrategy :: Set (Row, Column) -> Strategy -> Strategy
eliminateFromStrategy attacked (Strategy s) = Strategy $ Set.difference s attacked

placeQueen :: (MonadLogic m) => Problem -> (Row, Column) -> Partial -> m Partial
placeQueen problem (x, y) partial = do
  -- add a queen to the board
  let newQueens = Set.insert (x, y) partial.queens
  -- remove all the strategies which contains the new queen
  let strat1 = Set.filter (Set.notMember (x, y) . (.unStrategy)) partial.strategies
  -- from each strategy, remove the directly attacked candidates
  let attacked = directlyAttacked (x, y) problem
  let newStrategies = Set.map (eliminateFromStrategy attacked) strat1
  -- fail if this makes some strategies empty
  -- then we are out of candidates
  guard $ Set.notMember (Strategy Set.empty) newStrategies
  pure $
    Partial
      { queens = newQueens,
        strategies = newStrategies
      }

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Partial -> m (Row, Column)
candidate partial = do
  -- choose the strategy with the smallest set of candidates
  Strategy s <- case Set.lookupMin partial.strategies of
    Just strat -> pure strat
    Nothing -> empty -- no strategies left, fail
    -- choose a queen from the strategy
  choose s

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  ifte
    (candidate partial)
    ( \(x, y) -> do
        newPartial <- placeQueen problem (x, y) partial
        solve problem newPartial
    )
    (pure partial)

solution :: (MonadLogic m) => Problem -> m [(Row, Column)]
solution problem = do
  endState <- solve problem (mkPartial problem)
  -- Completeness: ensure enough queens were placed
  guard $ Set.size endState.queens == size problem
  pure (Set.toList endState.queens)

-- | Create an initial empty partial solution for the given problem
mkPartial :: Problem -> Partial
mkPartial problem =
  Partial
    { queens = Set.empty,
      strategies = Set.fromList $ map (Strategy . Set.fromList) (rowStrategies ++ columnStrategies ++ colorStrategies)
    }
  where
    n = size problem
    rowStrategies = [[(r, c) | c <- [0 .. n - 1]] | r <- [0 .. n - 1]]
    columnStrategies = [[(r, c) | r <- [0 .. n - 1]] | c <- [0 .. n - 1]]
    colorStrategies = [[(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], problem ! (i, j) == color] | color <- [0 .. n - 1]]