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
publicTests = testGroup "Public Tests" [publicMancalaTests, sampleGameTest]

sampleGameTest :: TestTree
sampleGameTest = testCaseSteps "Playing a sample game" $ \step -> do
  let game = Mancala.newGame

  step "Moving the third bowl"
  let (_, game2) = doMove 2 game
  [4, 4, 0, 5, 5, 5, 4, 4, 4, 4, 4, 4] @=? gameToList game2
  One @=? activePlayer game2
  1 @=? getScore game2 One
  0 @=? getScore game2 Two

  step "Moving the fourth bowl"
  let (_, game3) = doMove 3 game2
  [4, 4, 0, 0, 6, 6, 5, 5, 4, 4, 4, 4] @=? gameToList game3
  Two @=? activePlayer game3
  2 @=? getScore game3 One
  0 @=? getScore game3 Two

  step "Moving the ninth bowl"
  let (_, game4) = doMove 8 game3
  [4, 4, 0, 0, 6, 6, 5, 5, 0, 5, 5, 5] @=? gameToList game4
  Two @=? activePlayer game4
  2 @=? getScore game4 One
  1 @=? getScore game4 Two

  step "Moving the eighth bowl"
  let (_, game5) = doMove 7 game4
  [4, 4, 0, 0, 6, 6, 5, 0, 1, 6, 6, 6] @=? gameToList game5
  Two @=? activePlayer game5
  2 @=? getScore game5 One
  2 @=? getScore game5 Two

  step "Moving the twelfth bowl"
  let (_, game6) = doMove 11 game5
  [5, 5, 1, 1, 7, 6, 5, 0, 1, 6, 6, 0] @=? gameToList game6
  One @=? activePlayer game6
  2 @=? getScore game6 One
  3 @=? getScore game6 Two

  step "Moving the second bowl"
  let (_, game7) = doMove 1 game6
  [5, 0, 2, 2, 8, 7, 5, 0, 1, 6, 6, 0] @=? gameToList game7
  One @=? activePlayer game7
  3 @=? getScore game7 One
  3 @=? getScore game7 Two

  step "Moving the fifth bowl"
  let (_, game8) = doMove 4 game7
  [5, 0, 2, 2, 0, 8, 6, 1, 2, 7, 7, 1] @=? gameToList game8
  Two @=? activePlayer game8
  4 @=? getScore game8 One
  3 @=? getScore game8 Two

  step "Moving the twelfth bowl"
  let (_, game9) = doMove 11 game8
  [5, 0, 2, 2, 0, 8, 6, 1, 2, 7, 7, 0] @=? gameToList game9
  Two @=? activePlayer game9
  4 @=? getScore game9 One
  4 @=? getScore game9 Two

  step "Moving the seventh bowl"
  let (_, game10) = doMove 6 game9
  [5, 0, 2, 2, 0, 8, 0, 2, 3, 8, 8, 1] @=? gameToList game10
  Two @=? activePlayer game10
  4 @=? getScore game10 One
  5 @=? getScore game10 Two

  step "Moving the twelfth bowl"
  let (_, game11) = doMove 11 game10
  [5, 0, 2, 2, 0, 8, 0, 2, 3, 8, 8, 0] @=? gameToList game11
  Two @=? activePlayer game11
  4 @=? getScore game11 One
  6 @=? getScore game11 Two

  step "Moving the ninth bowl"
  let (_, game12) = doMove 8 game11
  [0, 0, 2, 2, 0, 8, 0, 2, 0, 9, 9, 0] @=? gameToList game12
  One @=? activePlayer game12
  4 @=? getScore game12 One
  12 @=? getScore game12 Two

  step "Moving the sixth bowl"
  let (_, game13) = doMove 5 game12
  [0, 0, 2, 2, 0, 0, 1, 3, 1, 10, 10, 0] @=? gameToList game13
  Two @=? activePlayer game13
  7 @=? getScore game13 One
  12 @=? getScore game13 Two

  step "Moving the eleventh bowl"
  let (_, game14) = doMove 10 game13
  [1, 1, 3, 3, 1, 1, 2, 4, 1, 10, 0, 1] @=? gameToList game14
  One @=? activePlayer game14
  7 @=? getScore game14 One
  13 @=? getScore game14 Two

  step "Moving the sixth bowl"
  let (_, game15) = doMove 5 game14
  [1, 1, 3, 3, 1, 0, 2, 4, 1, 10, 0, 1] @=? gameToList game15
  One @=? activePlayer game15
  8 @=? getScore game15 One
  13 @=? getScore game15 Two

  step "Moving the fourth bowl"
  let (_, game16) = doMove 3 game15
  [1, 1, 3, 0, 2, 1, 2, 4, 1, 10, 0, 1] @=? gameToList game16
  One @=? activePlayer game16
  9 @=? getScore game16 One
  13 @=? getScore game16 Two

  step "Moving the sixth bowl"
  let (_, game17) = doMove 5 game16
  [1, 1, 3, 0, 2, 0, 2, 4, 1, 10, 0, 1] @=? gameToList game17
  One @=? activePlayer game17
  10 @=? getScore game17 One
  13 @=? getScore game17 Two

  step "Moving the fifth bowl"
  let (_, game18) = doMove 4 game17
  [1, 1, 3, 0, 0, 1, 2, 4, 1, 10, 0, 1] @=? gameToList game18
  One @=? activePlayer game18
  11 @=? getScore game18 One
  13 @=? getScore game18 Two

  step "Moving the sixth bowl"
  let (_, game19) = doMove 5 game18
  [1, 1, 3, 0, 0, 0, 2, 4, 1, 10, 0, 1] @=? gameToList game19
  One @=? activePlayer game19
  12 @=? getScore game19 One
  13 @=? getScore game19 Two

  step "Moving the third bowl"
  let (_, game20) = doMove 2 game19
  [1, 1, 0, 1, 1, 0, 0, 4, 1, 10, 0, 1] @=? gameToList game20
  Two @=? activePlayer game20
  15 @=? getScore game20 One
  13 @=? getScore game20 Two

  step "Moving the twelfth bowl"
  let (_, game21) = doMove 11 game20
  [1, 1, 0, 1, 1, 0, 0, 4, 1, 10, 0, 0] @=? gameToList game21
  Two @=? activePlayer game21
  15 @=? getScore game21 One
  14 @=? getScore game21 Two

  step "Moving the tenth bowl"
  let (_, game22) = doMove 9 game21
  [2, 2, 1, 2, 2, 0, 0, 4, 1, 0, 1, 1] @=? gameToList game22
  One @=? activePlayer game22
  15 @=? getScore game22 One
  17 @=? getScore game22 Two

  step "Moving the fourth bowl"
  let (_, game23) = doMove 3 game22
  [2, 2, 1, 0, 3, 1, 0, 4, 1, 0, 1, 1] @=? gameToList game23
  Two @=? activePlayer game23
  15 @=? getScore game23 One
  17 @=? getScore game23 Two

  step "Moving the twelfth bowl"
  let (_, game24) = doMove 11 game23
  [2, 2, 1, 0, 3, 1, 0, 4, 1, 0, 1, 0] @=? gameToList game24
  Two @=? activePlayer game24
  15 @=? getScore game24 One
  18 @=? getScore game24 Two

  step "Moving the eleventh bowl"
  let (_, game25) = doMove 10 game24
  [0, 2, 1, 0, 3, 1, 0, 4, 1, 0, 0, 0] @=? gameToList game25
  One @=? activePlayer game25
  15 @=? getScore game25 One
  21 @=? getScore game25 Two

  step "Moving the sixth bowl"
  let (_, game') = doMove 5 game25
  [0, 2, 1, 0, 3, 0, 0, 4, 1, 0, 0, 0] @=? gameToList game'
  One @=? activePlayer game'
  16 @=? getScore game' One
  21 @=? getScore game' Two

  step "Moving the second bowl"
  let (_, game26) = doMove 1 game'
  [0, 0, 2, 0, 3, 0, 0, 4, 0, 0, 0, 0] @=? gameToList game26
  Two @=? activePlayer game26
  18 @=? getScore game26 One
  21 @=? getScore game26 Two

  step "Moving the eighth bowl"
  let (_, game27) = doMove 7 game26
  [0, 0, 2, 0, 3, 0, 0, 0, 1, 1, 1, 1] @=? gameToList game27
  One @=? activePlayer game27
  18 @=? getScore game27 One
  21 @=? getScore game27 Two

  step "Moving the third bowl"
  let (_, game28) = doMove 2 game27
  [0, 0, 0, 1, 4, 0, 0, 0, 1, 1, 1, 1] @=? gameToList game28
  Two @=? activePlayer game28
  18 @=? getScore game28 One
  21 @=? getScore game28 Two

  step "Moving the ninth bowl"
  let (_, game29) = doMove 8 game28
  [0, 0, 0, 1, 4, 0, 0, 0, 0, 2, 1, 1] @=? gameToList game29
  One @=? activePlayer game29
  18 @=? getScore game29 One
  21 @=? getScore game29 Two

  step "Moving the fourth bowl"
  let (_, game30) = doMove 3 game29
  [0, 0, 0, 0, 5, 0, 0, 0, 0, 2, 1, 1] @=? gameToList game30
  Two @=? activePlayer game30
  18 @=? getScore game30 One
  21 @=? getScore game30 Two

  step "Moving the twelfth bowl"
  let (_, game31) = doMove 11 game30
  [0, 0, 0, 0, 5, 0, 0, 0, 0, 2, 1, 0] @=? gameToList game31
  Two @=? activePlayer game31
  18 @=? getScore game31 One
  22 @=? getScore game31 Two

  step "Moving the tenth bowl"
  let (_, game32) = doMove 9 game31
  [0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 2, 1] @=? gameToList game32
  One @=? activePlayer game32
  18 @=? getScore game32 One
  22 @=? getScore game32 Two

  step "Moving the fifth bowl"
  let (_, game33) = doMove 4 game32
  [0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 2, 1] @=? gameToList game33
  Two @=? activePlayer game33
  19 @=? getScore game33 One
  22 @=? getScore game33 Two

  step "Moving the seventh bowl"
  let (_, game34) = doMove 6 game33
  [0, 0, 0, 0, 0, 1, 0, 2, 1, 0, 2, 1] @=? gameToList game34
  One @=? activePlayer game34
  19 @=? getScore game34 One
  22 @=? getScore game34 Two

  step "Moving the sixth bowl (final move) finishes the game"
  let (res, game35) = doMove 5 game34
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] @=? gameToList game35
  20 @=? getScore game35 One
  28 @=? getScore game35 Two
  GameOver @=? res

  step "Trying to do another move does nothing"
  let (res, game36) = doMove 0 game35
  gameToList game35 @=? gameToList game36
  activePlayer game35 @=? activePlayer game35
  getScore game35 One @=? getScore game36 One
  getScore game35 Two @=? getScore game36 Two
  GameOver @=? res

  step "no matter whose turn it is"
  let (res, game37) = doMove 11 game36
  gameToList game36 @=? gameToList game37
  activePlayer game36 @=? activePlayer game37
  getScore game36 One @=? getScore game37 One
  getScore game36 Two @=? getScore game37 Two
  GameOver @=? res



gameSetupTests :: TestTree
gameSetupTests = testGroup "Game Setup"
  [ testCase "Player one is the first player in a new Mancala game" $
      One @=? activePlayer Mancala.newGame
  , testCaseSteps "A new mancala game starts with 4 stones in each Bowl" $ \step -> do
      step "Each bowl should have 4 stones in it"
      --all ((4==) . getNumberOfStones) (mancalaBowls Mancala.newGame) @? "a Bowl doesn't have 4 stones in it"
      [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4] @=? gameToList Mancala.newGame

      step "There should be 48 stones in total"
      48 @=? sum (map getNumberOfStones (mancalaBowls Mancala.newGame))
  , testCase "Both players have 0 score in a new Mancala game" $ do
      0 @=? getScore Mancala.newGame One
      0 @=? getScore Mancala.newGame Two
  ]

moveTests :: TestTree
moveTests = testGroup "Move Tests"
  [ testCaseSteps "Moving the first bowl" $ \step -> do
      let (_, game) = doMove 0 Mancala.newGame

      step "decreases the number of stones"
      0 @=? getNumberOfStonesInBowl game 0

      step "and makes the second player active"
      Two @=? activePlayer game
  , testCaseSteps "Moving the third bowl" $ \step -> do
      let (_, game) = doMove 2 Mancala.newGame

      step " decreases the number of stones"
      0 @=? (getNumberOfStones . (!!2) . mancalaBowls $ game)

      step "and keeps the first player active"
      One @=? activePlayer game
  , testCaseSteps "Moving an empty bowl is impossible" $ \step -> do
      let (r, game) = doMove 0 Mancala.newGame

      step "Moving the first bowl works"
      Ok @=? r

      step "but moving it again leaves the game unchanged and gives a message"
      let (m, game') = doMove 6 game
      let (m, game'') = doMove 0 game'
      game' @=? game''
      CantMoveEmptyBowl @=? m
  , testCase "Moving a bowl that's not the active player's is impossible" $ do
      let (r, game) = doMove 6 Mancala.newGame
      NotYourTurn @=? r
  , testCase "Moving the first bowl distributes the stones accordingly" $ do
      let (_, game) = doMove 0 Mancala.newGame
      [0, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4] @=? gameToList game
  , testCase "Moving the second bowl distributes the stones accordingly" $ do
      let (_, game) = doMove 1 Mancala.newGame
      [4, 0, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4] @=? gameToList game
  , testCaseSteps "Moving the third bowl" $ \step -> do
      let (rs, game) = doMove 2 Mancala.newGame

      step "distributes the stones accordingly"
      [4, 4, 0, 5, 5, 5, 4, 4, 4, 4, 4, 4] @=? gameToList game

      step "and increases the score of the first player by one"
      1 @=? getScore game One
  , testCaseSteps "Moving the fourth bowl" $ \step -> do
      let (rs, game) = doMove 3 Mancala.newGame

      step "distributes the stones accordingly"
      [4, 4, 4, 0, 5, 5, 5, 4, 4, 4, 4, 4] @=? gameToList game

      step "and increases the score of the first player by one"
      1 @=? getScore game One
  , testCaseSteps "Moving the twelfth bowl" $ \step -> do
      let (rs, game) = doMove 11 (switchActivePlayer Mancala.newGame)

      step "distributes the stones accordingly"
      [5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 0] @=? gameToList game

      step "and increases the score of the second player by one"
      1 @=? getScore game Two
  ]

captureTests :: TestTree
captureTests = testGroup "Capture Tests"
  [ testCaseSteps "Capturing Stones: " $ \step -> do
      let (_, game) = doMove 5 Mancala.newGame

      step "moving the sixth bowl to create an empty space"
      [4, 4, 4, 4, 4, 0, 5, 5, 5, 4, 4, 4] @=? gameToList game

      let (_, game2) = doMove 9 game
      step "then moving the tenth bowl to set up the capture"
      [5, 4, 4, 4, 4, 0, 5, 5, 5, 0, 5, 5] @=? gameToList game2

      let (_, game3) = doMove 1 game2
      step "should make moving the second bowl capture stones"
      [5, 0, 5, 5, 5, 0, 0, 5, 5, 0, 5, 5] @=? gameToList game3

      step "and should increment the score accordingly"
      7 @=? getScore game3 One
   , testCaseSteps "Don't capture enemy stones: " $ \step -> do
      let (_, game) = doMove 0 Mancala.newGame

      step "after moving the first bowl to create an empty space"
      [0, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4] @=? gameToList game

      step "moving the tenth bowl should not capture stones"
      let (_, game') = doMove 9 game

      [1, 5, 5, 5, 5, 4, 4, 4, 4, 0, 5, 5] @=? gameToList game'
      1 @=? getScore game' Two
  ]

publicMancalaTests :: TestTree
publicMancalaTests = testGroup "Mancala Tests"
  [ gameSetupTests
  , moveTests
  , captureTests
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
gameToList = map getNumberOfStones . mancalaBowls

getScore :: Mancala -> Player -> Int
getScore m One = playerOneScore m
getScore m Two = playerTwoScore m
