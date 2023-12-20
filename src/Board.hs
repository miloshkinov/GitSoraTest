{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Board where  -- do NOT CHANGE export of module


-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars

--this has been changed 

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False


-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN boardString = figureCount boardString 0 0 && setUpChecker boardString 0 0

--tests if it's only components ',' and '/' and 'r' and 'b' and if it's the right amount of r and b since neither can be deleted throughout the game 
figureCount :: String -> Int -> Int -> Bool
figureCount [] countB countR = if (countB == 12) && (countR == 12) then True else False
figureCount (x:xs) countB countR = case x of
                'r' -> figureCount xs countB (countR + 1)
                'b'-> figureCount xs (countB + 1) countR
                ',' -> figureCount xs countB countR
                '/' -> figureCount xs countB countR
                _ -> False --because we only want the components to be /,rb 

--tests if it starts with 5 commas has 1 slash after every 5 Commas every time except for with the last 5 Commas (basically board structure)
setUpChecker :: String -> Int -> Int -> Bool
setUpChecker [] 5 5 = True
setUpChecker [] _ _ = False
setUpChecker (x:xs) commaCount slashCount= case x of
            ',' -> setUpChecker xs (commaCount+1) slashCount
            '/' -> if (commaCount == 5)
              then setUpChecker xs 0 (slashCount +1)
              else False
            _-> setUpChecker xs commaCount slashCount


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard fenString = iterateThroughRows fenString []

iterateThroughRows:: String -> Board -> Board
iterateThroughRows [] newBoard = newBoard
iterateThroughRows fenString newBoard = iterateThroughRows (deleteRow fenString) (newBoard ++ [goThroughRow fenString "" []])

deleteRow:: String -> String
deleteRow (x:xs)= case x of
    '/' -> xs
    _-> if (xs== []) then "" else deleteRow xs

goThroughRow :: String -> String -> [Cell] -> [Cell]
goThroughRow [] pieceString newSquareList = if (pieceString == "") then (newSquareList ++ [Empty])
                else newSquareList ++ (makeStack pieceString)
goThroughRow (x:xs) pieceString newSquareList =
        case x of
          'r'-> goThroughRow xs (pieceString ++ "r") newSquareList
          'b'-> goThroughRow xs (pieceString ++ "b") newSquareList
          ','-> goThroughRow xs "" (newSquareList ++ (makeStack pieceString))
          '/'-> newSquareList ++ (makeStack pieceString)

makeStack:: String -> [Cell]
makeStack ""= [Empty]
makeStack a= [Stack (figureList a [])]

figureList:: String -> [Player] -> [Player]
figureList [] liste= liste
figureList (x:xs) liste= case x of
  'r'-> figureList xs (liste ++ [Red])
  'b'-> figureList xs (liste ++ [Blue])

-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

path :: Pos -> Dir -> Int -> [Pos]
path position direction steps = manySteps position direction steps [position]

oneDirectionStep:: Pos -> Dir -> Pos
oneDirectionStep (Pos col row) dir= case dir of
      North-> Pos col (row +1)
      NorthEast-> Pos (succ col) (row +1)
      East-> Pos (succ col) row
      SouthEast-> Pos (succ col) (row -1)
      South-> Pos col (row -1)
      SouthWest-> Pos (pred col) (row -1)
      West-> Pos (pred col) row
      NorthWest-> Pos (pred col) (row +1)
      
--checks what the next field would be (result) if it's not on the board it reverses the direction 
--otherwise it adds it to list of positions and continues with next iteration at new position 
manySteps:: Pos -> Dir -> Int -> [Pos] -> [Pos]
manySteps start direction 1 positionsSoFar= let result= oneDirectionStep start direction in
                        if (isResultOffBoard result) then manySteps start (oppositeDir result direction) 1 positionsSoFar 
                        else positionsSoFar ++ [result]
manySteps start direction steps positionsSoFar=
                let result= oneDirectionStep start direction in
                if (isResultOffBoard result) then manySteps start (oppositeDir result direction) steps positionsSoFar 
                else manySteps result direction (steps-1) (positionsSoFar ++ [result])

--checks if position is off board 
isResultOffBoard:: Pos -> Bool 
isResultOffBoard (Pos col row)= col == '`'|| col == 'g'|| row == 7 || row == 0

oppositeDir:: Pos-> Dir -> Dir 
oppositeDir (Pos col row) direction= case direction of
      North-> South 
      NorthEast-> case (Pos col row) of 
        (Pos 'g' 7) -> SouthWest
        (Pos 'g' _) -> NorthWest
        (Pos _ 7) -> SouthEast 
      East-> West 
      SouthEast->  case (Pos col row) of 
        (Pos 'g' 0) -> NorthWest
        (Pos 'g' _) -> SouthWest
        (Pos _ 0) -> NorthEast 
      South-> North 
      SouthWest-> case (Pos col row) of 
        (Pos '`' 0) -> NorthEast
        (Pos '`' _) -> SouthEast
        (Pos _ 0) -> NorthWest
      West-> East
      NorthWest-> case (Pos col row) of 
        (Pos '`' 7) -> SouthEast
        (Pos '`' _) -> NorthEast
        (Pos _ 7) -> SouthWest