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

getBowl :: Mancala -> Int -> Bowl
getBowl = (!!) . getBowls

getOppositeIndex :: Int -> Int
getOppositeIndex = (11-)

belongsTo :: Player -> Int -> Bool
belongsTo One = (<6)
belongsTo Two = (>=6)

getNumberOfStonesInBowl :: Mancala -> Int -> Int
getNumberOfStonesInBowl = (getNumberOfStones .) . getBowl

numberOfStonesInBowl :: Int -> Mancala -> Int
numberOfStonesInBowl i = getNumberOfStones . (!!i) . getBowls

isBowlEmpty :: Mancala -> Int -> Bool
isBowlEmpty = (isEmpty .) . getBowl

playerSideEmpty :: Player -> Mancala -> Bool
playerSideEmpty One m = all (isBowlEmpty m) [0..5]
playerSideEmpty Two m = all (isBowlEmpty m) [6..11]

anySideEmpty :: Mancala -> Bool
anySideEmpty m = playerSideEmpty One m || playerSideEmpty Two m

updateBowl :: Int -> Bowl -> Mancala -> Mancala
updateBowl i newbowl m@(MancalaImpl bowls _ _ _) = m { getBowls = updateList i newbowl bowls }

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList 0 v (a:as) = v : as
updateList n v (a:as) = a : updateList (n - 1) v as

incrementBowl :: Int -> Mancala -> Mancala
incrementBowl i m = updateBowl i (addStone (getBowl m i)) m

incrementScore :: Player -> Int -> Mancala -> Mancala
incrementScore One n m@(MancalaImpl _ old _ _) = m { playerOneScore = old + n }
incrementScore Two n m@(MancalaImpl _ _ old _) = m { playerTwoScore = old + n }

type GameState a = State Mancala a

gameState :: (Mancala -> (a, Mancala)) -> GameState a
gameState = state

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
          | not (belongsTo (getActivePlayer game) i) = (NotYourTurn, game)
          | game `getNumberOfStonesInBowl` i /= 0 = runState (doEmptyBowl i >>= switchPlayers) game
          | otherwise = (CantMoveEmptyBowl, game)

switchPlayers :: MoveResult -> GameState MoveResult
switchPlayers Ok = gameState (\game ->
    (Ok, switchActivePlayer game)
  )
switchPlayers (KalahaEnd _) = return Ok
switchPlayers m = return m

switchActivePlayer :: Mancala -> Mancala
switchActivePlayer m@(MancalaImpl _ _ _ One) = m { getActivePlayer = Two}
switchActivePlayer m@(MancalaImpl _ _ _ Two) = m { getActivePlayer = One}

doEmptyBowl :: Int -> GameState MoveResult
doEmptyBowl i = do
  nstones <- gets $ numberOfStonesInBowl i
  modify $ updateBowl i emptyBowl
  passStones $ makeInitMessage i nstones

-- Dictates how a bowl/kalaha should pass stones
passStones :: Message -> GameState MoveResult
passStones m@(Message p i 0) = do
  nstones <- gets (numberOfStonesInBowl i)
  nstonesopp <- gets (numberOfStonesInBowl (getOppositeIndex i))
  maybeCapture $ CaptureMessage p i nstones nstonesopp
passStones (Message One 5 n) = takeStone $ KalahaMessage One n
passStones (Message Two 11 n) = takeStone $ KalahaMessage Two n
passStones (Message p i n) = takeStone $ Message p (rem (i + 1) 12) n
passStones (KalahaMessage p 0) = return $ KalahaEnd p
passStones (KalahaMessage One n) = takeStone $ Message One 6 n
passStones (KalahaMessage Two n) = takeStone $ Message Two 0 n

-- Dictates how a bowl/kalaha should handle being offered a stone
takeStone :: Message -> GameState MoveResult
takeStone (Message p i n) = do
  modify $ incrementBowl i
  passStones $ Message p i (n - 1)
takeStone (KalahaMessage p n) = do
  modify $ incrementScore p 1
  passStones $ KalahaMessage p (n - 1)

maybeCapture :: CaptureMessage -> GameState MoveResult
maybeCapture (CaptureMessage _ _ _ 0) = return Ok
maybeCapture (CaptureMessage p i 1 n)
  | belongsTo p i = do
      modify (updateBowl i emptyBowl)
      modify (updateBowl (getOppositeIndex i) emptyBowl)
      modify (incrementScore p (n + 1))
      return Ok
  | otherwise = return Ok
maybeCapture CaptureMessage {} = return Ok

{-
modify (incrementBowl i) >>
                            return (Message p i (n - 1)) >>=
                            passStones
gameState (\game ->
    let game' = updateBowl i emptyBowl game
        game'' = updateBowl (getOppositeIndex i) emptyBowl game'
        game''' = incrementScore p (n + 1) game''
    in (Ok, game'''))
-}
