module Attempt1 (
    solutions,
)

where

import Problem

import Control.Applicative
import Control.Monad.Logic
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

data Attempt = HasQueen | Eliminated
    deriving (Show, Eq)

type Partial = BoardOf Attempt

insertIfAbsent :: Ord k => k -> a -> Map k a -> Map k a
insertIfAbsent = Map.insertWith (flip const)

placeQueen :: Problem -> (Row, Column) -> Partial -> Partial
placeQueen problem (x, y) partial = 
          elimCorners problem.size (x, y) 
        . elimColumn problem.size y 
        . elimRow problem.size x 
        . maybe id (elimColor problem) cellColor 
        $ newBoard
    where
    newBoard = Map.insert (x, y) HasQueen partial
    cellColor = Map.lookup (x, y) problem.colors

elimCorners :: Int -> (Row, Column) -> Partial -> Partial
elimCorners size (x, y) partial = foldr (.) id elimFns $ partial
    where
    elimFns = [ insertIfAbsent (x', y') Eliminated | x' <- [x - 1, x + 1], y' <- [y - 1, y + 1], x' >= 0, x' < size, y' >= 0, y' < size ]

elimColumn :: Int -> Column -> Partial -> Partial
elimColumn size y partial = foldr elimRow' partial [0 .. size - 1]
    where
    elimRow' x' = insertIfAbsent (x', y) Eliminated

elimRow :: Int -> Row -> Partial -> Partial
elimRow size x partial = foldr elimColumn' partial [0 .. size - 1]
    where
    elimColumn' y' = insertIfAbsent (x, y') Eliminated

elimColor :: Problem -> Color -> Partial -> Partial
elimColor problem color = foldr (.) id elimFns
    where
        elimFns = [ insertIfAbsent (x, y) Eliminated | x <- [0 .. problem.size - 1], y <- [0 .. problem.size - 1], Map.lookup (x, y) problem.colors == Just color ]

genCandidates :: (MonadLogic m) => Int -> Partial -> m (Int, Int)
genCandidates size partial = foldr interleave empty [ pure (x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1], Map.lookup (x, y) partial == Nothing ]

solve :: (MonadLogic m) => Problem -> Partial -> m (Partial)
solve problem partial = do
    ifte 
        (genCandidates problem.size partial)
        (\ (x, y) -> do
            let newBoard = placeQueen problem (x, y) partial
            solve problem newBoard)
        (pure partial)

queenView :: Partial -> [(Row, Column)]
queenView partial = [(x, y) | ((x, y), HasQueen) <- Map.toList partial]

solutions :: (MonadLogic m) => Problem -> m ([(Row, Column)])
solutions problem = do 
    candidate <- solve problem (Map.empty)
    let queens = queenView candidate
    -- Completeness: ensure enough queens were placed
    guard (length queens == problem.size)
    pure queens