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

checkWinner :: GameState -> Maybe Player
checkWinner = undefined

makeMove :: GameState -> Move -> Maybe GameState
makeMove = undefined

makeLegalMove :: GameState -> Move -> GameState
makeLegalMove = undefined

isValidMove :: GameState -> Move -> Bool
isValidMove = undefined

getValidMoves :: GameState -> [Move]
getValidMoves = undefined

--let defaultBoard = undefined
