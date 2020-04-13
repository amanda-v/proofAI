module Game
  ( Game
  -- * Create
  , new
  -- * Game play
  , play, Error(..)
  , renew
  -- * Query
  , grid, turn
  , Outcome(..), outcome
  )
  where


import Grid as Grid
import Mark as Mark
import Referee as Referee


-- | An abstract data type for tracking a game of Tic-tac-toe. The API built
-- for it enforces the game logic of Tic-tac-toe.
data Game
  = Playing Grid Mark
  | GameOver Grid Mark Outcome


-- | Useful for debugging purposes.
--
-- >>> show game2
-- "..o.x....; x"
instance Show Game where
  show (Playing grid mark) =
    show grid ++ "; " ++ show mark

  show (GameOver grid mark Win) =
    show grid ++ "; " ++ show mark ++ "; win"

  show (GameOver grid mark Squash) =
    show grid ++ "; " ++ show mark ++ "; squash"


-- | Starts a new game of Tic-tac-toe such that the first player uses the given
-- mark. For e.g.
--
-- >>> let game = new O
--
-- The grid managed by @game@ will be empty and the first player will use the
-- mark @O@.
new :: Mark -> Game
new = Playing Grid.empty


-- | Suppose it's @X@'s turn. Then, 'play' attempts to place @X@ at the given
-- position on the grid.
--
-- 1. If the position is not within the boundaries of the grid then
--    @Left 'OutOfBounds'@ is returned.
--
-- 2. If a mark already occupies the position then @Left 'Unavailable'@ is
--    returned.
--
-- 3. Otherwise the mark is placed at the position and an updated game is
--    returned.
play :: Position -> Game -> Either Error Game
play p (Playing grid mark) =
  if Grid.inBounds p then 
    if Grid.isAvailable p grid then
      Right (playMark p mark grid)
    else 
      Left Unavailable
  else
    Left OutOfBounds
play _ game = Right game


playMark :: Position -> Mark -> Grid -> Game
playMark p mark grid =
  let
    nextGrid = Grid.set p mark grid
  in
    case Referee.unsafeDecide nextGrid mark of
      Nothing ->
        Playing nextGrid (Mark.swap mark)

      Just outcome ->
        GameOver nextGrid mark outcome


-- | The possible errors that can occur when attempting to play the game.
data Error
  = OutOfBounds
  | Unavailable
  deriving (Eq, Show)


-- | Starts a new game. If the game is squashed then the first player of the
-- new game is changed, otherwise the first player of the new game is whoever
-- had the next play or won in the given game.
renew :: Game -> Game
renew (Playing _ m) = new m
renew (GameOver _ m Win) = new m
renew (GameOver _ m Squash) = new (Mark.swap m)


-- | Returns the grid that's managed by the game. The returned grid is always
-- valid.
grid :: Game -> Grid
grid (Playing g _) = g
grid (GameOver g _ _) = g


-- | The returned mark is to be interpreted in one of three ways:
--
--   * If the game is not over then the returned mark represents the mark of
--     the player to play next.
--   * If the game is over and one of the players won then the returned mark
--     represents the mark of the winning player.
--   * If the game is over and squashed then the returned mark represents the
--     mark of the player that squashed the game.
turn :: Game -> Mark
turn (Playing _ m) = m
turn (GameOver _ m _) = m


-- | Returns whether or not the game is over and why.
--
-- It will be one of @Nothing@ (which means the game is not over), @Just 'Win'@
-- or @Just 'Squash'@.
outcome :: Game -> Maybe Outcome
outcome (GameOver _ _ o) = Just o
outcome _ = Nothing
