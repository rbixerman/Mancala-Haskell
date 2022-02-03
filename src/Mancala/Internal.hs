module Mancala.Internal (
  module Mancala.Bowl,
  module Mancala.Internal
  ) where

import Mancala.Bowl

data Mancala = MancalaImpl {
  mancalaBowls :: ![Bowl],
  playerOneScore :: !Int,
  playerTwoScore :: !Int,
  activePlayer :: !Player
} deriving (Eq, Show)

data Player = One | Two deriving (Eq, Show)

newGame :: Mancala
newGame = MancalaImpl (replicate 12 defaultBowl) 0 0 One

getBowl :: Mancala -> Int -> Bowl
getBowl = (!!) . mancalaBowls

getOppositeIndex :: Int -> Int
getOppositeIndex = (11-)

belongsTo :: Player -> Int -> Bool
belongsTo One = (<6)
belongsTo Two = (>=6)

getNumberOfStonesInBowl :: Mancala -> Int -> Int
getNumberOfStonesInBowl = (getNumberOfStones .) . getBowl

numberOfStonesInBowl :: Int -> Mancala -> Int
numberOfStonesInBowl i = getNumberOfStones . (!!i) . mancalaBowls

isBowlEmpty :: Mancala -> Int -> Bool
isBowlEmpty = (isEmpty .) . getBowl

playerSideEmpty :: Player -> Mancala -> Bool
playerSideEmpty One m = all (isBowlEmpty m) [0..5]
playerSideEmpty Two m = all (isBowlEmpty m) [6..11]

anySideEmpty :: Mancala -> Bool
anySideEmpty m = playerSideEmpty One m || playerSideEmpty Two m

updateBowl :: Int -> Bowl -> Mancala -> Mancala
updateBowl i newbowl m@MancalaImpl{mancalaBowls = bowls} = m { mancalaBowls = updateList i newbowl bowls }

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList 0 v (a:as) = v : as
updateList n v (a:as) = a : updateList (n - 1) v as

incrementBowl :: Int -> Mancala -> Mancala
incrementBowl i m = updateBowl i (addStone (getBowl m i)) m

incrementScore :: Player -> Int -> Mancala -> Mancala
incrementScore One n m@MancalaImpl{ playerOneScore = old } = m { playerOneScore = old + n }
incrementScore Two n m@MancalaImpl{ playerTwoScore = old } = m { playerTwoScore = old + n }

switchActivePlayer :: Mancala -> Mancala
switchActivePlayer m@MancalaImpl{ activePlayer = One} = m { activePlayer = Two}
switchActivePlayer m@MancalaImpl{ activePlayer = Two} = m { activePlayer = One}
