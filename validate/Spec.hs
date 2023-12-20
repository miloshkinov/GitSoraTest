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

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testPath

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
