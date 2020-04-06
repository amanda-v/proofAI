module Run.Player
  ( Player(..)
  , numHumans
  )
  where


data Player
  = Human
  | Computer


instance Show Player where
  show Human = "You"
  show Computer = "Bot"

numHumans :: Player -> Player -> Int
numHumans Human Human = 2
numHumans Human Computer = 1
numHumans Computer Human = 1
numHumans Computer Computer = 0
