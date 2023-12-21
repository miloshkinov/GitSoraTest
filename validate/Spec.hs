-- #############################################################################
-- ###### VALIDATION TESTS                                            ##########
-- ###### (DO NOT CHANGE ANYTHING)                                    ##########
-- ###### Note: execute tests using "stack test deathstacks:validate  ##########
-- #############################################################################

import Test.Hspec

import Board
    ( buildBoard,
      path,
      validateFEN,
      Board,
      Cell(Empty,Stack),
      Player(Red, Blue),
      Pos(Pos), Dir (North))

import Deathstacks ( playerWon, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testPath
    testPlayerWon
    testPossibleMoves
    testIsValidMove
    testListMoves

sampleBoard :: Board
sampleBoard = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN ..." $ do
        it "empty string is not valid" $ do
            validateFEN "" `shouldBe` (False :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "IF Validate-Module-Board: buildBoard ..." $ do
        it "build empty board" $ do
            (buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,") `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)

testPath :: Spec
testPath = describe "IF Validate-Module-Board: path ..." $ do
        it "one step north" $ do
            path (Pos 'c' 2) North 1 `shouldBe` ([(Pos 'c' 2), (Pos 'c' 3)] :: [Pos])

testPlayerWon :: Spec
testPlayerWon = describe "IF Validate-Module-Game: playerWon ..." $ do
        it "start board not finished" $ do
            playerWon sampleBoard `shouldBe` (Nothing :: Maybe Player)       

testPossibleMoves :: Spec
testPossibleMoves = describe "IF Validate-Module-Game: possibleMoves ..." $ do
        it "single test" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1] :: [Move])

testIsValidMove :: Spec
testIsValidMove = describe "IF Validate-Module-Game: isValidMove ..." $ do
        it "simple valid move" $ do
            let b = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)

testListMoves :: Spec
testListMoves = describe "IF Validate-Module-Game: listMoves ..." $ do
        it "red cannot move" $ do
            let board = [[Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                result = []
                in
                    map show (listMoves board Red) `shouldMatchList` (result :: [String])
