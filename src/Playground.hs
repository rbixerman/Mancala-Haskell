module Playground where

import Control.Monad

data Mancala

newtype GameState a = GameState {
  runState :: Mancala -> (a, Mancala)
}

data Move

data Error = InvalidMove
data Outcomes = Ok
type Arg = Either Move Error
type MoveResult = Error

gameState :: (Mancala -> (a, Mancala)) -> GameState a
gameState = GameState

instance Functor GameState where
  fmap = liftM

instance Applicative GameState where
  pure = return
  (<*>) = ap

instance Monad GameState where
  return a = gameState $ \s -> (a, s)
  first >>= second = gameState $ \state ->
    let (res, s1) = runState first state
    in runState (second res) s1

doConcreteMove :: Int -> Mancala -> (MoveResult, Mancala)
doConcreteMove = runState . getMoveProcessor

isValidMove :: Int -> Mancala -> Bool
isValidMove = undefined

getMoveProcessor :: Int -> GameState MoveResult
getMoveProcessor i = gameState $ \state ->
  if isValidMove i state then
    (InvalidMove, state)
  else
    runState (emptyBowlAt i) state

emptyBowlAt :: Int -> GameState b
emptyBowlAt i = undefined

passStones :: b -> GameState c
passStones = undefined

captureIfNeeded :: c -> GameState MoveResult
captureIfNeeded = undefined
