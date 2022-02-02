{-# LANGUAGE TupleSections #-}
module Mancala.Internal where

import Control.Monad
import Control.Monad.State.Lazy
import Mancala.Bowl

data Mancala = MancalaImpl {
  getBowls :: [Bowl],
  playerOneScore :: Int,
  playerTwoScore :: Int,
  getActivePlayer :: Player
} deriving (Eq, Show)

newGame :: Mancala
newGame = MancalaImpl (replicate 12 defaultBowl) 0 0 One

updateBowl :: Mancala -> Int -> Bowl -> Mancala
updateBowl (MancalaImpl bowls one two p) idx newbowl = MancalaImpl (updateList idx newbowl bowls) one two p

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList 0 v (a:as) = v : as
updateList n v (a:as) = a : updateList (n - 1) v as

incrementBowl :: Mancala -> Int -> Mancala
incrementBowl m i = updateBowl m i (addStone (getBowls m !! i))

getNumberOfStonesInBowl :: Mancala -> Int -> Int
getNumberOfStonesInBowl = (getNumberOfStones . ) . (!!) . getBowls

incrementScore :: Mancala -> Player -> Int -> Mancala
incrementScore m@(MancalaImpl _ old _ _) One n = m { playerOneScore = old + n }
incrementScore m@(MancalaImpl _ _ old _) Two n = m { playerTwoScore = old + n }

type GameState a = State Mancala a

gameState :: (Mancala -> (a, Mancala)) -> GameState a
gameState = state

setOutput :: a -> GameState a
setOutput a = gameState (a,)

data Player = One | Two deriving (Eq, Show)

getScore :: Mancala -> Player -> Int
getScore m One = playerOneScore m
getScore m Two = playerTwoScore m

data MoveResult = CantMoveEmptyBowl | GameOver | NotYourTurn | Ok | KalahaEnd Player
  deriving (Show, Eq)

data Message = Message Player Int Int | KalahaMessage Player Int
data CaptureMessage = CaptureMessage Player Int Int Int

makeInitMessage :: Int -> Int -> Message
makeInitMessage idx stones =
  if idx < 6 then
    Message One idx stones
  else
    Message Two idx stones

doMove :: Int -> Mancala -> (MoveResult, Mancala)
doMove i = runState (getMoveProcessor idx) where idx = rem i 12

getMoveProcessor :: Int -> GameState MoveResult
getMoveProcessor i = gameState f
  where f game
          | not (belongsTo i (getActivePlayer game)) = (NotYourTurn, game)
          | game `getNumberOfStonesInBowl` i /= 0 = runState (doEmptyBowl i >>= switchPlayers) game
          | otherwise = (CantMoveEmptyBowl, game)

switchPlayers :: MoveResult -> GameState MoveResult
switchPlayers Ok = gameState (\game ->
    (Ok, switchActivePlayer game)
  )
switchPlayers (KalahaEnd _) = setOutput Ok
switchPlayers m = setOutput m

switchActivePlayer :: Mancala -> Mancala
switchActivePlayer m@(MancalaImpl _ _ _ One) = m { getActivePlayer = Two}
switchActivePlayer m@(MancalaImpl _ _ _ Two) = m { getActivePlayer = One}

doEmptyBowl :: Int -> GameState MoveResult
doEmptyBowl i = gameState (\game ->
  let nstones =  game `getNumberOfStonesInBowl` i
  in (makeInitMessage i nstones, updateBowl game i emptyBowl)
  ) >>= passStones

-- Dictates how a bowl/kalaha should pass stones
passStones :: Message -> GameState MoveResult
passStones m@(Message p i 0) = gameState (\game ->
  (CaptureMessage p i (getNumberOfStonesInBowl game i) (getNumberOfStonesInBowl game (getOppositeIndex i)), game)
  ) >>= maybeCapture
passStones (Message One 5 n) = takeStone $ KalahaMessage One n
passStones (Message Two 11 n) = takeStone $ KalahaMessage Two n
passStones (Message p i n) = takeStone $ Message p (rem (i + 1) 12) n
passStones (KalahaMessage p 0) = setOutput $ KalahaEnd p
passStones (KalahaMessage One n) = takeStone $ Message One 6 n
passStones (KalahaMessage Two n) = takeStone $ Message Two 0 n

-- Dictates how a bowl/kalaha should handle being offered a stone
takeStone :: Message -> GameState MoveResult
takeStone (Message p i n) = gameState (\game ->
  (Message p i (n - 1), game `incrementBowl` i)
  ) >>= passStones
takeStone (KalahaMessage p n) = gameState (\game ->
  (KalahaMessage p (n - 1), incrementScore game p 1)
  ) >>= passStones

getOppositeIndex :: Int -> Int
getOppositeIndex i = 11 - i

maybeCapture :: CaptureMessage -> GameState MoveResult
maybeCapture (CaptureMessage _ _ _ 0) = setOutput Ok
maybeCapture (CaptureMessage p i 1 n)
  | belongsTo i p = gameState (\game ->
      let game' = updateBowl game i emptyBowl
          game'' = updateBowl game' (getOppositeIndex i) emptyBowl
          game''' = incrementScore game'' p (n + 1)
      in (Ok, game''')
    ) >>= setOutput
  | otherwise = setOutput Ok
maybeCapture CaptureMessage {} = setOutput Ok

belongsTo :: Int -> Player -> Bool
belongsTo i One = i < 6
belongsTo i Two = i >= 6
