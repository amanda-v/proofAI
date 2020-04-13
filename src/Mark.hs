-- | @X@s and @O@s.
module Mark (Mark(..), swap) where


-- | A player can mark a space using either an @X@ or an @O@.
data Mark = X | O deriving Eq


-- | >>> show X
-- "x"
--
-- >>> show O
-- "o"
instance Show Mark where
  show X = "x"
  show O = "o"


-- | Switches one mark for the other.
--
-- >>> swap X
-- <interactive>:178:2-5: error:
--     Variable not in scope: swap :: t0 -> t
-- <BLANKLINE>
-- <interactive>:178:7: error: Data constructor not in scope: X
--
-- >>> swap O
-- <interactive>:182:2-5: error:
--     Variable not in scope: swap :: t0 -> t
-- <BLANKLINE>
-- <interactive>:182:7: error: Data constructor not in scope: O
--
swap :: Mark -> Mark
swap X = O
swap O = X
