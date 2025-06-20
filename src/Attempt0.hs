module Attempt0
  ( solution,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.Set (Set)
import qualified Data.Set as Set
import Problem

type Partial = Set (Row, Column)

choose :: (MonadLogic m, Foldable t) => t a -> m a
choose = foldr ((<|>) . pure) empty

candidate :: (MonadLogic m) => Problem -> Partial -> m (Row, Column)
candidate problem partial = do
  let n = size problem
  x <- choose [0 .. n - 1]
  y <- choose [0 .. n - 1]
  -- this cell should not have been already picked
  guard $ (x, y) `Set.notMember` partial
  pure (x, y)

sound :: Problem -> Partial -> Bool
sound problem partial = soundRows && soundColumns && soundColors && soundCorners
  where
    soundRows = all (soundRow partial) [0 .. size problem - 1]
    soundColumns = all (soundColumn partial) [0 .. size problem - 1]
    soundColors = all (soundColor problem partial) [0 .. size problem - 1]
    soundCorners = all (soundCorner partial) partial

complete :: Problem -> Partial -> Bool
complete problem partial = Set.size partial == size problem

soundRow :: Partial -> Row -> Bool
soundRow partial row = Set.size (Set.filter (\(i, _) -> i == row) partial) <= 1

soundColumn :: Partial -> Column -> Bool
soundColumn partial col = Set.size (Set.filter (\(_, j) -> j == col) partial) <= 1

soundColor :: Problem -> Partial -> Color -> Bool
soundColor problem partial color =
  Set.size (Set.filter (\(i, j) -> problem ! (i, j) == color) partial) <= 1

soundCorner :: Partial -> (Row, Column) -> Bool
soundCorner partial (x, y) = northWest && northEast && southWest && southEast
  where
    northWest = Set.notMember (x - 1, y - 1) partial
    northEast = Set.notMember (x - 1, y + 1) partial
    southWest = Set.notMember (x + 1, y - 1) partial
    southEast = Set.notMember (x + 1, y + 1) partial

solve :: (MonadLogic m) => Problem -> Partial -> m Partial
solve problem partial =
  if complete problem partial
    then pure partial
    else do
      ifte -- if-then-else
        (candidate problem partial)
        ( \(x, y) -> do
            let newBoard = Set.insert (x, y) partial
            solve problem newBoard
        )
        (pure partial)

solution :: (MonadLogic m) => Problem -> m [(Row, Column)]
solution problem = do
  endState <- solve problem Set.empty
  guard $ sound problem endState
  pure (Set.toList endState)