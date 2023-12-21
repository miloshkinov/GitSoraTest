-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################

import Test.Hspec

import Board
    ( buildBoard,
      path,
      validateFEN,
      Board,
      Cell(Empty,Stack),
      Player(Red, Blue),
      Pos(Pos), Dir (North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest))

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testPath

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN ..." $ do
        it "Valid board is valid" $ do
                validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` (True ::  Bool)
        it "Empty string is not valid" $ do
            validateFEN "" `shouldBe` (False :: Bool)
        it "Slash in front is not valid" $ do
                validateFEN "/rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` (False :: Bool)
        it "Slash in back is not valid" $ do
                validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb/" `shouldBe` (False :: Bool)
        it "Double Slash in one section is not valid" $ do
                validateFEN "rr,,,,,rr/,,/,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb/" `shouldBe` (False :: Bool)
        it "Random Character is not valid" $ do
                validateFEN "rr,,,,,rr/a,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb/" `shouldBe` (False :: Bool)
        it "Wrong amount of Commas in total is not valid" $ do
                validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb,/" `shouldBe` (False :: Bool)
        it "Wrong amount of Commas in one section is not valid" $ do
                validateFEN "rr,,,,,rr/,,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb/" `shouldBe` (False :: Bool)
        it "Wrong amount of red pieces is not valid" $ do
                validateFEN "rrr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` (False :: Bool)
        it "Wrong amount of blue pieces is not valid" $ do
                    validateFEN "rrb,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` (False :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "IF Validate-Module-Board: buildBoard ..." $ do
        it "build empty board" $ do
            (buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,") `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)
        it "build sample board" $ do
            (buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb") `shouldBe` [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
        it "build double check sample board" $ do
            (buildBoard ",rr,rr,rr,rr,rr/rb,,,,,/,,,,,/,,,,,/,,,,,br/,bb,bb,bb,bb,bb") `shouldBe` [[Empty,Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Stack [Red,Blue],Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Stack [Blue,Red]],[Empty,Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]


testPath :: Spec
testPath = describe "IF Validate-Module-Board: path" $ do
        it "check c3 on Board (Multiple steps but no edge of board or startposition reached)" $ do
               path (Pos 'c' 3) NorthEast 2 `shouldBe` ([Pos 'c' 3, Pos 'd' 4, Pos 'e' 5]::[Pos])
        it "check e2 on Board (Normal Bounce) 2 NorthEast" $ do 
                path (Pos 'e' 2) NorthEast 2 `shouldBe` ([Pos 'e' 2, Pos 'f' 3, Pos 'e' 4]::[Pos])
        it "check f5 on edge" $ do
                path (Pos 'f' 5) NorthEast 1 `shouldBe` ([Pos 'f' 5, Pos 'e' 6]::[Pos])
        it "check a1 on edge Southwest with multiple steps is d4" $ do
                path (Pos 'a' 1) SouthWest 3 `shouldBe` ([Pos 'a' 1, Pos 'b' 2, Pos 'c' 3, Pos 'd' 4]::[Pos])
        ---Opposite dir north, south, west, east 
        it "check b5 North with multiple steps is b4" $ do
                path (Pos 'b' 5) North 3 `shouldBe` ([Pos 'b' 5, Pos 'b' 6, Pos 'b' 5, Pos 'b' 4]::[Pos])
        it "check b5 West with multiple steps is b4" $ do
                path (Pos 'b' 5) West 3 `shouldBe` ([Pos 'b' 5, Pos 'a' 5, Pos 'b' 5, Pos 'c' 5]::[Pos])
        it "check f5 East with multiple steps is d5" $ do
                path (Pos 'f' 5) East 2 `shouldBe` ([Pos 'f' 5, Pos 'e' 5, Pos 'd' 5]::[Pos]) 
        it "check South with multiple steps is d5" $ do
                path (Pos 'e' 1) South 2 `shouldBe` ([Pos 'e' 1, Pos 'e' 2, Pos 'e' 3]::[Pos]) 
        ----all corners 
        it "check NorthEast corner" $ do
                path (Pos 'f' 6) NorthEast 1 `shouldBe` ([Pos 'f' 6, Pos 'e' 5]::[Pos])
        it "check SouthEast corner" $ do
                path (Pos 'f' 1) SouthEast 1 `shouldBe` ([Pos 'f' 1, Pos 'e' 2]::[Pos])
        it "check NorthWest corner" $ do
                path (Pos 'a' 6) NorthWest 1 `shouldBe` ([Pos 'a' 6, Pos 'b' 5]::[Pos])
        ---- 
        it "check NorthEast with multiple steps and bounces" $ do
                path (Pos 'd' 5) NorthEast 3 `shouldBe` ([Pos 'd' 5, Pos 'e' 6, Pos 'f' 5, Pos 'e' 4]::[Pos])
        --
        it "check SouthEast with  bottom edge" $ do
                path (Pos 'c' 2) SouthEast 2 `shouldBe` ([Pos 'c' 2, Pos 'd' 1, Pos 'e' 2]::[Pos])  
        --
        it "check SouthWest left edge" $ do
                path (Pos 'a' 3) SouthWest 1 `shouldBe` ([Pos 'a' 3, Pos 'b' 2]::[Pos])  
        it "check SouthWest bottom edge" $ do
                path (Pos 'b' 1) SouthWest 1 `shouldBe` ([Pos 'b' 1, Pos 'a' 2]::[Pos])  
        --
        it "check NorthWest left" $ do
                path (Pos 'a' 4) NorthWest 3 `shouldBe` ([Pos 'a' 4, Pos 'b' 5,Pos 'c' 6,Pos 'd' 5]::[Pos])  
        it "check NorthWest top" $ do
                path (Pos 'c' 6) NorthWest 1 `shouldBe` ([Pos 'c' 6, Pos 'b' 5]::[Pos])  


