data Player = Red | Black | None deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Piece]]
type Coordinate = (Integer,Integer)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)

instance Show Board where
  show board = undefined
  

checkWinner :: GameState -> Player

makeMove :: GameState -> Move -> GameState

makeLegalMove :: GameState -> Move -> GameState

isValidMove :: GameState -> Move -> Boolean

getValidMoves :: GameState -> [Move]

let defaultBoard = undefined
