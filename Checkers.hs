import Data.List

data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Integer,Integer)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)

--Helper function for prettySHow that will turn a row of pieces into a string
writeRow :: [Maybe Piece] -> String
writeRow ((Just piece):xs) = 
  let symbol = case piece of
                 (Red,Peasent) -> "r"
                 (Red,Emperor) -> "R"
                 (Black,Peasent) -> "b"
                 (Black,Emperor) -> "B"
  in " " ++ symbol ++ " |" ++ writeRow xs
writeRow ((Nothing):xs) = "   |" ++ writeRow xs
writeRow [] = []

prettyShow :: GameState -> String
prettyShow (player,board) = intercalate "\n" $ reverse $ [(show player) ++ "'s Turn"] ++ (aux 1 board)
  where size = length board
        aux num (row:rowTail) = [(show num) ++ " |" ++ (writeRow row), "   -----------------------------"] ++ aux (num + 1) rowTail
        aux num [] = ["   1   2   3   4   5   6   7   8 "]


--checkWinner happens at the start of a "turn"
--takes the current gamestate and the moves the current player can make
--this means that if no moves can be made the 'other' player wins 
checkWinner :: GameState -> [Move] -> Maybe Player
checkWinner (Red,board) [] = Just Black
checkWinner (Black,board) [] = Just Red
checkWinner gs moves = Nothing


makeMove :: GameState -> Move -> Maybe GameState
makeMove = undefined

makeLegalMove :: GameState -> Move -> GameState
makeLegalMove = undefined

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
