data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Integer,Integer)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)
    
prettyShow :: GameState -> String
prettyShow = undefined

checkWinner :: GameState -> Maybe Player
checkWinner = undefined

makeMove :: GameState -> Move -> Maybe GameState
makeMove = undefined

makeLegalMove :: GameState -> Move -> GameState
makeLegalMove = undefined

getPieceAtIndex :: GameState -> Coordinate -> Maybe Piece
getPieceAtIndex (player,board) (x,y) = 
  let xIndex = if ((y % 2) == 0) then (x + 1)/2 else x/2
  in drop xIndex-1 (take xIndex (drop y-1 (take y board)))

isValidMove :: GameState -> Move -> Bool
isValidMove (player,board) move = False

getValidMoves :: GameState -> [Move]
getValidMoves (player,board) = undefined

defaultBoard = 
  [
    [Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent)],
    [(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing],
    [Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent),Nothing,(Black,Peasent)],
    [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
    [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
    [(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing],
    [Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent)],
    [(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing,(Red,Peasent),Nothing]
  ]
