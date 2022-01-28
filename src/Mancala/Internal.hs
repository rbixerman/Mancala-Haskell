module Mancala.Internal where

import Control.Monad
import Control.Monad.State.Lazy
import Mancala.Bowl

newtype Mancala = MancalaImpl {
  getBowls :: [Bowl]
} deriving (Eq, Show)

newGame :: Mancala
newGame = MancalaImpl (replicate 12 (Bowl 4))

updateBowl :: Int -> Bowl -> Mancala -> Mancala
updateBowl i b m = MancalaImpl (updateList i b (getBowls m))

type GameState a = State Mancala a

gameState :: (Mancala -> (a, Mancala)) -> GameState a
gameState = state

data Player = One | Two deriving (Eq, Show)

getActivePlayer :: Mancala -> Player
getActivePlayer _ = One

getScore :: Player -> Mancala -> Int
getScore _ _ = 0

data MoveResult = GameAlreadyOver | CantMoveEmptyBowl
  | PassStones Int Int  | Ok
  deriving (Show, Eq)

doMove :: Int -> Mancala -> (MoveResult, Mancala)
doMove i m = runState (getMoveProcessor i) m

doEmptyBowl :: Int -> GameState MoveResult
doEmptyBowl i = gameState $ \state ->
  let nstones =  getNumberOfStones $ getBowls state !! i
  in if nstones == 0 then
       (CantMoveEmptyBowl, state)
     else
       (PassStones (i + 1) nstones, updateBowl i emptyBowl state)

getMoveProcessor :: Int -> GameState MoveResult
getMoveProcessor i = (doEmptyBowl i) >>= passStones 

passStones :: MoveResult -> GameState MoveResult
passStones (PassStones _ 0) = gameState (\s -> (Ok, s))
passStones (PassStones i n) = gameState (\state ->
  let nstate = updateBowl i (addStone (getBowls state !! i)) state
  in (PassStones (i + 1) (n - 1), nstate)
  ) >>= passStones
passStones a = gameState (\ s -> (a, s))

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
