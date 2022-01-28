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

data MoveResult = GameAlreadyOver
  | CantMoveEmptyBowl
  | PassStones { idx :: Int, nstones :: Int }
  | PlayerOneKalaha Int
  | Ok
  deriving (Show, Eq)

doMove :: Int -> Mancala -> (MoveResult, Mancala)
doMove i = runState (getMoveProcessor i)

getMoveProcessor :: Int -> GameState MoveResult
getMoveProcessor i = doEmptyBowl i >>= passStones

doEmptyBowl :: Int -> GameState MoveResult
doEmptyBowl i = gameState $ \state ->
  let nstones =  getNumberOfStones $ getBowls state !! i
  in if nstones == 0 then
       (CantMoveEmptyBowl, state)
     else
       (PassStones (i + 1) nstones, updateBowl i emptyBowl state)

passStones :: MoveResult -> GameState MoveResult
passStones (PassStones _ 0) = setOutput Ok
passStones (PassStones 5 nstones) = gameState (\state ->
  let nstate = updateBowl 5 (addStone (getBowls state !! 5)) state
  in (PlayerOneKalaha (nstones - 1), nstate)
  ) >>= updateScore
passStones (PassStones idx nstones) = gameState (\state ->
  let nstate = updateBowl idx (addStone (getBowls state !! idx)) state
  in (PassStones (idx+1) (nstones-1), nstate)
  ) >>= passStones
passStones fallThrough = setOutput fallThrough -- Things we cannot handle just silently pass through

updateScore :: MoveResult -> GameState MoveResult
updateScore (PlayerOneKalaha 0) = setOutput Ok
updateScore (PlayerOneKalaha n) = gameState (\state ->
  let pOneScore = getScore One state + 1
  in (PassStones 6 (n -1), state { playerOneScore = pOneScore})
  ) >>= passStones
updateScore fallThrough = setOutput fallThrough

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
