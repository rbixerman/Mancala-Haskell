module Mancala.Bowl where

newtype Bowl = Bowl Int deriving (Eq, Show)

getNumberOfStones :: Bowl -> Int
getNumberOfStones (Bowl n) = n

addStone :: Bowl -> Bowl
addStone (Bowl n) = Bowl (n + 1)

defaultBowl :: Bowl
defaultBowl = Bowl 4

emptyBowl :: Bowl
emptyBowl = Bowl 0

isEmpty :: Bowl -> Bool
isEmpty = (emptyBowl==)

takeAll :: Bowl -> Bowl
takeAll _ = emptyBowl
