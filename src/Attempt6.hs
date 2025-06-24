module Attempt6
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Array (Array, array)
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
    strategies :: Set Strategy,
    directlyAttacked :: Array (Row, Column) (Set (Row, Column))
  }
  deriving (Show, Eq)

eliminateFromStrategy :: Set (Row, Column) -> Strategy -> Strategy
eliminateFromStrategy attacked (Strategy s) = Strategy $ Set.difference s attacked

placeQueen :: (MonadLogic m) => (Row, Column) -> Partial -> m Partial
placeQueen (x, y) partial = do
  -- add a queen to the board
  let newQueens = Set.insert (x, y) partial.queens
  -- remove all the strategies which contains the new queen
  let strat1 = Set.filter (Set.notMember (x, y) . (.unStrategy)) partial.strategies
  -- from each strategy, remove the directly attacked candidates
  let attacked = partial.directlyAttacked ! (x, y)
  let newStrategies = Set.map (eliminateFromStrategy attacked) strat1
  -- fail if this makes some strategies empty
  -- then we are out of candidates
  guard $ Set.notMember (Strategy Set.empty) newStrategies
  pure $
    partial
      { queens = newQueens,
        strategies = newStrategies
      }

placeManyQueens :: (MonadLogic m) => Problem -> Set (Row, Column) -> Partial -> m Partial
placeManyQueens problem positions partial = do
  -- check if the set is sound
  guard $ sound problem positions
  -- add all queens to the board
  let newQueens = Set.union positions partial.queens
  -- remove all the strategies which contains any of the new queens
  let strat1 = Set.filter (Set.disjoint positions . (.unStrategy)) partial.strategies
  -- from each strategy, remove the directly attacked candidates
  let attacked = Set.unions $ Set.map (partial.directlyAttacked !) positions
  let newStrategies = Set.map (eliminateFromStrategy attacked) strat1
  -- fail if this makes some strategies empty
  -- then we are out of candidates
  guard $ Set.notMember (Strategy Set.empty) newStrategies
  pure $
    partial
      { queens = newQueens,
        strategies = newStrategies
      }

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Partial -> m (Row, Column)
candidate partial = do
  -- choose the strategy with the smallest set of candidates
  Strategy s <- maybe empty pure (Set.lookupMin partial.strategies)
  -- choose a queen from the strategy
  choose s

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  let super = superstrategy partial
  if not $ Set.null super
    then do
      -- if there are super strategies, place all queens from them
      newPartial <- placeManyQueens problem super partial
      solve problem newPartial
    else
      -- otherwise, continue with the usual candidate selection
      ifte
        (candidate partial)
        ( \pos -> do
            newPartial <- placeQueen pos partial
            solve problem newPartial
        )
        (pure partial)

-- A superstrategy is a union of all singleton strategies
superstrategy :: Partial -> Set (Row, Column)
superstrategy partial = Set.unions $ Set.map (.unStrategy) $ Set.takeWhileAntitone (\(Strategy s) -> Set.size s == 1) partial.strategies

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
      strategies = Set.fromList $ map (Strategy . Set.fromList) (rowStrategies ++ columnStrategies ++ colorStrategies),
      directlyAttacked =
        array
          ((0, 0), (n - 1, n - 1))
          [ ((r, c), directly (r, c))
            | r <- [0 .. n - 1],
              c <- [0 .. n - 1]
          ]
    }
  where
    n = size problem
    rowStrategies = [[(r, c) | c <- [0 .. n - 1]] | r <- [0 .. n - 1]]
    columnStrategies = [[(r, c) | r <- [0 .. n - 1]] | c <- [0 .. n - 1]]
    colorStrategies = [[(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], problem ! (i, j) == color] | color <- [0 .. n - 1]]
    directly (x, y) =
      Set.fromList $
        [ (i, j)
          | i <- [0 .. n - 1],
            j <- [0 .. n - 1],
            (i, j) /= (x, y),
            isDirectlyAttacked problem (x, y) (i, j)
        ]

sound :: Problem -> Set (Row, Column) -> Bool
sound problem queens = all (\p -> all (\q -> p == q || not (isDirectlyAttacked problem p q)) queens) queens

isDirectlyAttacked :: Problem -> (Row, Column) -> (Row, Column) -> Bool
isDirectlyAttacked problem (i, j) (x, y) =
  i == x -- same row
    || j == y -- same column
    || abs (i - x) == 1 && abs (j - y) == 1 -- diagonally touching
    || problem ! (i, j) == problem ! (x, y) -- same color