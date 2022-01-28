import Test.Tasty
import Test.Tasty.HUnit

import Mancala
import Mancala.Internal
import Mancala.Bowl

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [publicTests, internalTests]

publicTests :: TestTree
publicTests = testGroup "Public Tests" [publicMancalaTests]

publicMancalaTests :: TestTree
publicMancalaTests = testGroup "Mancala Tests"
  [ testCase "Player one is the first player in a new Mancala game" $
      One @=? getActivePlayer Mancala.newGame

  , testCaseSteps "A new mancala game starts with 4 stones in each Bowl" $ \step -> do
      step "Each bowl should have 4 stones in it"
      --all ((4==) . getNumberOfStones) (getBowls Mancala.newGame) @? "a Bowl doesn't have 4 stones in it"
      [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4] @=? gameToList Mancala.newGame

      step "There should be 48 stones in total"
      48 @=? sum (map getNumberOfStones (getBowls Mancala.newGame))

  , testCase "Emptying the first bowl decreases the number of stones" $ do
      let (_, game) = doMove 0 Mancala.newGame
      0 @=? (getNumberOfStones . head . getBowls $ game)
  , testCase "Emptying the third bowl decreases the number of stones" $ do
      let (_, game) = doMove 2 Mancala.newGame
      0 @=? (getNumberOfStones . (!!2) . getBowls $ game)

  , testCase "Trying to move an empty bowl returns appropriate move result" $ do
      let (_, game) = doMove 0 Mancala.newGame
      let (m, game') = doMove 0 game
      game @=? game'
      CantMoveEmptyBowl @=? m

  , testCase "Moving the first bowl distributes the stones" $ do
      let (_, game) = doMove 0 Mancala.newGame
      [0, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4] @=? gameToList game

  , testCase "Moving the second bowl distributes the stones" $ do
      let (_, game) = doMove 1 Mancala.newGame
      [4, 0, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4] @=? gameToList game
  , testCaseSteps "Moving the third bowl" $ \step -> do
      let (rs, game) = doMove 2 Mancala.newGame

      step "distributes the stones accordingly"
      [4, 4, 0, 5, 5, 5, 4, 4, 4, 4, 4, 4] @=? gameToList game

      step "and increases the score of the first player by one"
      1 @=? getScore One game 
  , testCase "Both players have 0 score in a new Mancala game" $ do
      0 @=? getScore One Mancala.newGame
      0 @=? getScore Two Mancala.newGame
  ]

internalTests :: TestTree
internalTests = testGroup "Internal Tests" [bowlTests]

bowlTests :: TestTree
bowlTests = testGroup "Bowl Tests"
  [ testCase "A default Bowl starts with 4 stones" $
      4 @=? getNumberOfStones defaultBowl

  , testCase "When adding a stone to a default Bowl we have 5 stones" $
      5 @=? getNumberOfStones (addStone defaultBowl)

  , testCase "When taking all stones we get an empty bowl" $
      0 @=? getNumberOfStones (takeAll defaultBowl)
  ]

gameToList :: Mancala -> [Int]
gameToList = map getNumberOfStones . getBowls
