module Mancala (
  Mancala,
  Player(..),
  Bowl,
  MoveResult(..),
  newGame,
  mancalaBowls,
  activePlayer,
  playerOneScore,
  playerTwoScore,
  doMove,
  isEmpty,
  getNumberOfStones
  ) where


import Control.Monad
import Control.Monad.State.Lazy

import Mancala.Internal

data MoveResult = CantMoveEmptyBowl | GameOver | NotYourTurn | Ok | KalahaEnd Player
  deriving (Show, Eq)

data Message = Message Player Int Int | KalahaMessage Player Int

data CaptureMessage = CaptureMessage Player Int Int Int

type GameState a = State Mancala a

gameState :: (Mancala -> (a, Mancala)) -> GameState a
gameState = state
doMove :: Int -> Mancala -> (MoveResult, Mancala)
doMove i = runState $ getMoveProcessor i where idx = rem i 12

getMoveProcessor :: Int -> GameState MoveResult
getMoveProcessor i = do
    game <- get
    let yourBowl = belongsTo (activePlayer game) i
    let action | anySideEmpty game = cleanUp
               | not yourBowl = pure NotYourTurn
               | isBowlEmpty game i = pure CantMoveEmptyBowl
               | otherwise = move i
    action

cleanUp :: GameState MoveResult
cleanUp = modify addPlayerBowlsToScore >> pure GameOver

addPlayerBowlsToScore :: Mancala -> Mancala
addPlayerBowlsToScore m@(MancalaImpl bowls pOneOld pTwoOld _) =
    m { mancalaBowls = emptyBowls
      , playerOneScore = pOneOld + pOneSum
      , playerTwoScore = pTwoOld + pTwoSum}
  where
    pOneSum = foldr ((+) . getNumberOfStones) 0 (take 6 bowls)
    pTwoSum = foldr ((+) . getNumberOfStones) 0 (drop 6 bowls)
    emptyBowls = replicate 12 emptyBowl

move :: Int -> GameState MoveResult
move i = do
    nstones <- doEmptyBowl i
    moveResult <- passStones $ makeInitMessage i nstones
    gameEnd <- gets anySideEmpty
    if gameEnd
      then cleanUp
      else switchPlayers moveResult

doEmptyBowl :: Int -> GameState Int
doEmptyBowl i = do
    nstones <- gets (numberOfStonesInBowl i)
    modify $ updateBowl i emptyBowl
    pure nstones

makeInitMessage :: Int -> Int -> Message
makeInitMessage idx stones = if idx < 6
    then Message One idx stones
    else Message Two idx stones

passStones :: Message -> GameState MoveResult
passStones (Message p i 0) = do
    nstones <- gets (numberOfStonesInBowl i)
    nstonesopp <- gets (numberOfStonesInBowl (getOppositeIndex i))
    maybeCapture $ CaptureMessage p i nstones nstonesopp
passStones (Message One 5 n)     = takeStone $ KalahaMessage One n
passStones (Message Two 11 n)    = takeStone $ KalahaMessage Two n
passStones (Message p i n)       = takeStone $ Message p (rem (i + 1) 12) n
passStones (KalahaMessage p 0)   = pure $ KalahaEnd p
passStones (KalahaMessage One n) = takeStone $ Message One 6 n
passStones (KalahaMessage Two n) = takeStone $ Message Two 0 n

takeStone :: Message -> GameState MoveResult
takeStone (Message p i n) = do
    modify $ incrementBowl i
    passStones $ Message p i (n - 1)
takeStone (KalahaMessage p n) = do
    modify $ incrementScore p 1
    passStones $ KalahaMessage p (n - 1)

maybeCapture :: CaptureMessage -> GameState MoveResult
maybeCapture (CaptureMessage _ _ _ 0) = pure Ok
maybeCapture (CaptureMessage p i 1 n)
    | belongsTo p i = do
        modify (updateBowl i emptyBowl)
        modify (updateBowl (getOppositeIndex i) emptyBowl)
        modify (incrementScore p (n + 1))
        pure Ok
    | otherwise = pure Ok
maybeCapture CaptureMessage {} = pure Ok

switchPlayers :: MoveResult -> GameState MoveResult
switchPlayers m = case m of
    Ok -> modify switchActivePlayer >> pure Ok
    m -> pure m
