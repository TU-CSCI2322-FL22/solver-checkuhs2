{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use !!" #-}
import Data.List

data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Int,Int)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)


--writeShow is a helper function for prettyShow that will turn a row of pieces into a string
writeRow :: Integer -> [Maybe Piece] -> String
writeRow num ((Just piece):xs) = 
  let symbol = case piece of
                 (Red,Peasent) -> "r"
                 (Red,Emperor) -> "R"
                 (Black,Peasent) -> "b"
                 (Black,Emperor) -> "B"
  in if num `mod` 2 == 1
    then "   | " ++ symbol ++ " |" ++ (writeRow num xs)
  else
    " " ++ symbol ++ " |   |" ++ (writeRow num xs) 

writeRow num ((Nothing):xs) = "   |   |" ++ (writeRow num xs)
writeRow num [] = []

--prettyShow takes a game state and returns a human readable string representing the Gamestate
--NOTE - when testing in ghci, new lines characters will not appear unless you pass the string from prettyShow into putStrLn
--For example, if you want to test the default board in ghci, you should write: putStrLn $ prettyShow defaultBoard
prettyShow :: GameState -> String
prettyShow (player,board) = intercalate "\n" $ reverse $ ["\n", (show player) ++ "'s Turn", "\n", "  ---------------------------------"] ++ (aux 1 board) ++ ["\n"]
  where size = length board
        aux num (row:rowTail) = [(show num) ++ " |" ++ (writeRow num row), "  ---------------------------------"] ++ aux (num + 1) rowTail
        aux num [] = ["    1   2   3   4   5   6   7   8 "]


--checkWinner happens at the start of a "turn"
--takes the current gamestate and the moves the current player can make
--this means that if no moves can be made the 'other' player wins 
checkWinner :: GameState -> [Move] -> Maybe Player
checkWinner (Red,board) [] = Just Black
checkWinner (Black,board) [] = Just Red
checkWinner gs moves = Nothing


makeMove :: GameState -> Move -> [Move] -> Maybe GameState
makeMove gs move [] = Nothing
makeMove gs move possibleMoves =
  if move `elem` possibleMoves then Just $ makeLegalMove gs move else Nothing

makeLegalMove :: GameState -> Move -> GameState
makeLegalMove gs@(player,board) [] = gs
makeLegalMove gs@(player,board) ((s,e):ms) =
  if abs (getRow s - getRow e) == 2 
  then makeLegalMove (player, makeJumpMove board (s,e)) ms
  else makeLegalMove (player, makeMoveMove board (s,e)) ms
  where makeJumpMove :: Board -> (Coordinate,Coordinate) -> Board
        makeJumpMove bd (s,e) = 
          let jumpedCoords = if even $ getRow s --if its on an even row
                             then if getCol e > getCol s {-jumped right on even row-} 
                             then (getCol s, (getRow s + getRow e)`div`2) else (getCol e,(getRow s + getRow e)`div`2)
                             else if getCol e > getCol s {-jumped right on odd row-} 
                             then (getCol e, (getRow s + getRow e)`div`2) else (getCol s,(getRow s + getRow e)`div`2)
              piece = head $ drop (getCol s) (head $ drop (7 - getRow s) bd)
          in updateBoard (updateBoard (updateBoard bd Nothing s) Nothing jumpedCoords) piece e
        makeMoveMove :: Board -> (Coordinate,Coordinate) -> Board
        makeMoveMove bd (s,e) = 
          let piece = head $ drop (getCol s) (head $ drop (7 - getRow s) bd)
          in updateBoard (updateBoard bd Nothing s) piece e
        updateBoard :: Board -> Maybe Piece -> Coordinate -> Board
        updateBoard bd piece (col,row) = 
          let (rowsBefore,changedRow:rowsAfter) = splitAt (7-row) bd
              (spacesBefore,_:spacesAfter) = splitAt col changedRow
              newRow = spacesBefore ++ piece : spacesAfter
          in rowsBefore ++ newRow : rowsAfter


getCol :: Coordinate -> Int
getCol = fst
getRow :: Coordinate -> Int
getRow = snd

isValidMove :: GameState -> Move -> Bool
isValidMove = undefined

getValidMoves :: GameState -> [Move]
getValidMoves = undefined

f :: Char -> Maybe Piece
f 'n' = Nothing
f 'r' = Just(Red,Peasent)
f 'b' = Just(Black,Peasent)

defaultBoard =
  [
    map f "bbbb",
    map f "bbbb",
    map f "bbbb",
    map f "nnnn",
    map f "nnnn",
    map f "rrrr",
    map f "rrrr",
    map f "rrrr"
  ]

defaultGame = (Black,defaultBoard)

{-
|||||(0,7)|||||(1,7)|||||(2,7)|||||(3,7)
(0,6)|||||(1,6)|||||(2,6)|||||(3,6)|||||
|||||(0,5)|||||(1,5)|||||(2,5)|||||(3,5)
(0,4)|||||(1,4)|||||(2,4)|||||(3,4)|||||
|||||(0,3)|||||(1,3)|||||(2,3)|||||(3,3)
(0,2)|||||(1,2)|||||(2,2)|||||(3,2)|||||
|||||(0,1)|||||(1,1)|||||(2,1)|||||(3,1)
(0,0)|||||(1,0)|||||(2,0)|||||(3,0)|||||
-}
