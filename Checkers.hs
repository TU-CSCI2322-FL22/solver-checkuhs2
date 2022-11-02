data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Integer,Integer)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)
  
prettyShow :: GameState -> String

checkWinner :: GameState -> Maybe Player

makeMove :: GameState -> Move -> Maybe GameState

makeLegalMove :: GameState -> Move -> GameState

isValidMove :: GameState -> Move -> Boolean

getValidMoves :: GameState -> [Move]

let defaultBoard = undefined
