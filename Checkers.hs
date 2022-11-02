data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Integer,Integer)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)
  
prettyShow :: GameState -> String

checkWinner :: GameState -> Maybe Player
checkWinner gs@(player, board) = 
  let
  in 
  where checkNoMoves :: Boolean
        checkNoMoves = null $ getValidMoves gs
        checkNoPieces :: Boolean
        checkNoPieces = 

makeMove :: GameState -> Move -> Maybe GameState

makeLegalMove :: GameState -> Move -> GameState

isValidMove :: GameState -> Move -> Boolean

getValidMoves :: GameState -> [Move]

let defaultBoard = 
  [
    [Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent)]
    [(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing]
    [Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent)]
    [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
    [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
    [(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing]
    [Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent)]
    [(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing]
  ]
