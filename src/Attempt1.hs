module Attempt1
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Problem

data Attempt = HasQueen | Eliminated
  deriving (Show, Eq)

type Partial = Map (Row, Column) Attempt

insertIfAbsent :: (Ord k) => k -> a -> Map k a -> Map k a
insertIfAbsent = Map.insertWith (\_ x -> x)

placeQueen :: Problem -> (Row, Column) -> Partial -> Partial
placeQueen problem (x, y) partial =
  elimCorners (x, y) problem
    . elimColumn y problem
    . elimRow x problem
    . elimColor color problem
    $ newBoard
  where
    newBoard = Map.insert (x, y) HasQueen partial
    color = problem ! (x, y)

elimCorners :: (Row, Column) -> Problem -> Partial -> Partial
elimCorners (x, y) problem = foldr (.) id elimFns
  where
    n = size problem
    elimFns =
      [ insertIfAbsent (x', y') Eliminated
        | x' <- [x - 1, x + 1],
          x' >= 0,
          x' < n,
          y' <- [y - 1, y + 1],
          y' >= 0,
          y' < n
      ]

elimColumn :: Column -> Problem -> Partial -> Partial
elimColumn y problem partial = foldr elimRow' partial [0 .. size problem - 1]
  where
    elimRow' x' = insertIfAbsent (x', y) Eliminated

elimRow :: Row -> Problem -> Partial -> Partial
elimRow x problem partial = foldr elimColumn' partial [0 .. size problem - 1]
  where
    elimColumn' y' = insertIfAbsent (x, y') Eliminated

elimColor :: Color -> Problem -> Partial -> Partial
elimColor color problem = foldr (.) id elimFns
  where
    n = size problem
    elimFns = [insertIfAbsent (x, y) Eliminated | x <- [0 .. n - 1], y <- [0 .. n - 1], problem ! (x, y) == color]

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
candidate problem partial = do
  let n = size problem
  x <- choose [0 .. n - 1]
  y <- choose [0 .. n - 1]
  -- only pick unmarked cells
  guard (isNothing (Map.lookup (x, y) partial))
  pure (x, y)

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial = do
  ifte -- if-then-else
    (candidate problem partial)
    ( \pos -> do
        let newBoard = placeQueen problem pos partial
        solve problem newBoard
    )
    (pure partial)

queenView :: Partial -> [(Row, Column)]
queenView partial = [(x, y) | ((x, y), status) <- Map.toList partial, status == HasQueen]

solution :: (MonadLogic m) => Problem -> m [(Row, Column)]
solution problem = do
  endState <- solve problem Map.empty
  let queens = queenView endState
  -- Completeness: ensure enough queens were placed
  guard (length queens == size problem)
  pure queens