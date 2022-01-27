import Test.Tasty
import Test.Tasty.HUnit

import Mancala
import Mancala.Internal

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
      all ((4==) . getNumberOfStones) (getBowls Mancala.newGame) @? "a Bowl doesn't have 4 stones in it"

      step "There should be 48 stones in total"
      48 @=? sum (map getNumberOfStones (getBowls Mancala.newGame))
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
