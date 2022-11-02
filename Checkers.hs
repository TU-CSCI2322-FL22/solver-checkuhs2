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
                 (Red,Emporer) -> "R"
                 (Black,Peasent) -> "b"
                 (Black,Peasent) -> "B"
  in " " ++ symbol ++ " |" ++ writeRow xs
writeRow ((Nothing):xs) = "   |" ++ writeRow xs
writeRow [] = []

prettyShow :: GameState -> String
prettyShow (player,board) = 
  where size = length board
        aux num (row:rowTail) = [(show num) ++ " |" ++ writeRow row] ++ aux (num + 1) rowTail
        aux num [] = ["   1   2   3   4   5   6   7   8 "]

checkWinner :: GameState -> Maybe Player

makeMove :: GameState -> Move -> Maybe GameState

makeLegalMove :: GameState -> Move -> GameState

isValidMove :: GameState -> Move -> Boolean

getValidMoves :: GameState -> [Move]

let defaultBoard = undefined
