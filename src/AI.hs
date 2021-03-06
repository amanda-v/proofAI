-- | The computer's \"brain\".
--
-- This module exports a functon called 'getPositions' that uses a
-- <https://en.wikipedia.org/wiki/Minimax minimax> algorithm to determine the
-- best positions to place a mark based on the configuration of a game's grid.
module AI (getPositions) where

import Game as Game
import Grid as Grid
import Mark as Mark
import Referee 


type Score = Int
type Depth = Int
type Result = (Score, Depth, [Position])


getPositions :: Game -> [Position]
getPositions game
  | n == 9 = availablePositions
  | n == 1 = availablePositions
  | otherwise = positions (maximize max min 0 grid)
  where
    n = length availablePositions
    positions (_, _, ps) = ps

    max = Game.turn game
    min = Mark.swap max

    availablePositions = Grid.availablePositions grid
    grid = Game.grid game

-- | Returns the best positions to place a mark based on the configuration of
-- the given game's grid.
--
-- In general, it uses a <https://en.wikipedia.org/wiki/Minimax minimax>
-- algorithm. However, there are two cases where an analysis is not needed:
--
-- 1. When every position is available then every position is immediately
--    returned.
--
-- 2. When only one position is available then that position is immediately
--    returned.

maximize :: Mark -> Mark -> Depth -> Grid -> Result
maximize max min depth grid = 
  case Referee.unsafeDecide grid min of
    Nothing ->
      let
        positions = Grid.availablePositions grid
        set p = Grid.set p max grid
        nextGrids = map set positions
        mins = map (minimize min max (depth+1)) nextGrids
        combine ((s, d, _), p) = (s, d, [p])
        nextResults = map combine (zip mins positions)
      in
        foldr1 maxResult nextResults

    Just outcome ->
      (minScore outcome, depth, [])


minimize :: Mark -> Mark -> Depth -> Grid -> Result
minimize min max depth grid =
  case Referee.unsafeDecide grid max of
    Nothing ->
      let
        positions = Grid.availablePositions grid
        set p = Grid.set p min grid
        nextGrids = map set positions
        maxs = map (maximize max min (depth+1)) nextGrids
        combine ((s, d, _), p) = (s, d, [p])
        nextResults = map combine (zip maxs positions)
      in
        foldr1 minResult nextResults

    Just outcome ->
      (maxScore outcome, depth, [])


maxResult :: Result -> Result -> Result
maxResult r1@(s1, d1, ps1) r2@(s2, d2, ps2)
  | s1 > s2 = r1
  | s2 > s1 = r2
  | d1 < d2 = r1
  | d2 < d1 = r2
  | otherwise = (s1, d1, ps1 ++ ps2)


minResult :: Result -> Result -> Result
minResult r1@(s1, d1, ps1) r2@(s2, d2, ps2)
  | s1 < s2 = r1
  | s2 < s1 = r2
  | d1 < d2 = r1
  | d2 < d1 = r2
  | otherwise = (s1, d1, ps1 ++ ps2)


maxScore :: Outcome -> Score
maxScore Win = 2
maxScore Squash = 1


minScore :: Outcome -> Score
minScore = negate . maxScore
