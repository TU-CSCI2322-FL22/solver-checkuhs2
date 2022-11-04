data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasent deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Integer,Integer)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)
  
prettyShow :: GameState -> String
prettyShow = undefined


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
    map f "nbnbnbnb",
    map f "bnbnbnbn",
    map f "nbnbnbnb",
    map f "nnnnnnnn",
    map f "nnnnnnnn",
    map f "rnrnrnrn",
    map f "nrnrnrnr",
    map f "rnrnrnrn"
  ]
