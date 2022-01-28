module Mancala (
  Mancala,
  newGame,
  getActivePlayer,
  getBowls
  ) where

import Mancala.Internal

{- What is considered part of the public API of a mancala game∷

In any case, anyone (or anything) interfacing with the game is most likely
interested in∷
  - Getting the number of stones in each bowl (display/decision making)
  - Getting the current score of each player  (display/decision making)
  - Being able to know whose turn it is, or rather whether it is *our* turn
  - Telling the game to do a move
  - knowing the result of that move∷
    - invalid moves generally just do nothing and don't change the internals
      of the mancala black box, but some feedback on why the move is invalid can be nice
    - valid moves result in an updated internal state, which in principle we don't
      care about much either, but again feedback can be nice (for example, informing
      the user they get to do another move, although this could be derived locally)
  - knowing if the game has ended (this coincides with knowing whether anyone
    has a valid move)

  - It looks like we can use the state monad to tackle this; we have a finite
    (but very large) set of states, and every state has 6/12/14 (depending on
    what we want to handle as input) edges to other state nodes (possible their own),
    with some output indicating the result of the move.

    The idea would be that
      doMove :: Int -> MancalaState -> (MoveResult, MancalaState)
    by itself does not fit the state monad, but by partially applying we get
      (doMove Int) :: MancalaState -> (MoveResult, MancalaState)

    Bogus input would default to (doMove Int) = (,) InvalidMove
    for non-bogus input, we would have to create this function dynamically

    winning or terminal states should then have their own result as some GameAlreadyOver result,
    to indicate that we should start querying the state for relevant info about the results
    of the game, instead of trying to play more moves. When we are in a non-terminal state
    this result should never occur and all results should be informative.
-}



-- ideally we want this where Int is the bowl we want to move. In essence, our input
{-doMove :: Int -> Mancala -> (MoveResult, Mancala)
doMove = runState . pickMoveProcessor

pickMoveProcessor :: Int -> GameState MoveResult
pickMoveProcessor = undefined
-}
{- On the other hand, we could also consider doMove as saying; bind this state processor

doMove2 :: Int -- ^
  -> GameState a -- ^
  -> GameState MoveResult
doMove2 i = (>> pickMoveProcessor i)
-}
-- The benefit of this latter approach is that the game is essentially replayable
-- then when we return the GameState MoveResult;
-- I think in the public api, we forego the use of the state monad, and just do simple
-- query int mancala -> (MoveResult, Mancala)
-- then we can use the state monad internally
-- if there are any benefits, this should be clear while programming the internals
-- and then we can hopefully decide.





-- Random list rotate functions to have a 'circular' list, perhaps useful for
-- the bowls later.
rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight (x:xs) = xs ++ [x]

rotN :: [a] -> Int -> [a]
rotN = (!!) . iterate rotateRight
