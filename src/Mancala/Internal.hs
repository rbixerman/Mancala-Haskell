module Mancala.Internal where

data Mancala = MancalaImpl {
} deriving Show

data Player = One | Two deriving (Eq, Show)

newGame :: Mancala
newGame = MancalaImpl

getBowls :: Mancala -> [Bowl]
getBowls _ = replicate 12 defaultBowl

getActivePlayer :: Mancala -> Player
getActivePlayer _ = One

getScore :: Player -> Mancala -> Int
getScore _ _ = 0


newtype Bowl = Bowl Int deriving (Eq, Show)

takeAll :: Bowl -> Bowl
takeAll _ = Bowl 0

getNumberOfStones :: Bowl -> Int
getNumberOfStones (Bowl n) = n

addStone :: Bowl -> Bowl
addStone (Bowl n) = Bowl (n + 1)

defaultBowl :: Bowl
defaultBowl = Bowl 4
