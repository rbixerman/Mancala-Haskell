module Mancala.Internal where

import Control.Monad
import Control.Monad.State.Lazy
import Mancala.Bowl

data Mancala = MancalaImpl {
  getBowls :: [Bowl],
  playerOneScore :: Int,
  playerTwoScore :: Int
} deriving (Eq, Show)

newGame :: Mancala
newGame = MancalaImpl (replicate 12 (Bowl 4)) 0 0

updateBowl :: Int -> Bowl -> Mancala -> Mancala
updateBowl i b m = m { getBowls = updateList i b (getBowls m) }

type GameState a = State Mancala a

gameState :: (Mancala -> (a, Mancala)) -> GameState a
gameState = state

setOutput :: a -> GameState a
setOutput a = gameState $ \ s -> (a, s)

data Player = One | Two deriving (Eq, Show)

getActivePlayer :: Mancala -> Player
getActivePlayer _ = One

getScore :: Player -> Mancala -> Int
getScore One = playerOneScore
getScore Two = playerTwoScore

data MoveResult = CantMoveEmptyBowl | Ok
  deriving (Show, Eq)

data PassStones = PassStones Int Int
newtype PlayerOneKalaha = PlayerOneKalaha Int

doMove :: Int -> Mancala -> (MoveResult, Mancala)
doMove i = runState (getMoveProcessor i)

isValidMove :: Int -> Mancala -> Bool
isValidMove n game = nstones /= 0
  where nstones = getNumberOfStones $ getBowls game !! n

getMoveProcessor :: Int -> GameState MoveResult
getMoveProcessor i = gameState $ \state ->
  if isValidMove i state then
    runState (doEmptyBowl i) state
  else
    (CantMoveEmptyBowl, state)

doEmptyBowl :: Int -> GameState MoveResult
doEmptyBowl i = gameState (\state ->
  let nstones =  getNumberOfStones $ getBowls state !! i
  in (PassStones (i + 1) nstones, updateBowl i emptyBowl state)
  ) >>= passStones

passStones :: PassStones -> GameState MoveResult
passStones (PassStones _ 0) = setOutput Ok
passStones (PassStones 5 nstones) = gameState (\state ->
  let nstate = updateBowl 5 (addStone (getBowls state !! 5)) state
  in (PlayerOneKalaha (nstones - 1), nstate)
  ) >>= updateScore
passStones (PassStones idx nstones) = gameState (\state ->
  let nstate = updateBowl (red idx) (addStone (getBowls state !! red idx)) state
  in (PassStones (idx+1) (nstones-1), nstate)
  ) >>= passStones

red :: Int -> Int
red i = mod i 12

updateScore :: PlayerOneKalaha -> GameState MoveResult
updateScore (PlayerOneKalaha 0) = setOutput Ok
updateScore (PlayerOneKalaha n) = gameState (\state ->
  let pOneScore = getScore One state + 1
  in (PassStones 6 (n -1), state { playerOneScore = pOneScore})
  ) >>= passStones

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList 0 v (a:as) = v : as
updateList n v (a:as) = a : updateList (n - 1) v as

toZero :: Int -> [Bowl] -> [Bowl]
toZero i = updateList i emptyBowl

{-moveBowlZero = gameState (\state ->
  let firstBowl = head . getBowls $ state
  in case firstBowl of
    Bowl 0 -> (CantMoveEmptyBowl, state)
    _ -> (Normal, takeAll2 0 state)
  )
takeAll2 :: Int -> Mancala -> Mancala
takeAll2 n = MancalaImpl . toZero n . getBowls
-}
