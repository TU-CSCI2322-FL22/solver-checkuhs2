{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use !!" #-}
import Data.List
import Data.Maybe (isNothing)

data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasant deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [[Maybe Piece]]
type Coordinate = (Int,Int)
type Move = [(Coordinate,Coordinate)]
type GameState = (Player,Board)

--writeShow is a helper function for prettyShow that will turn a row of pieces into a string
writeRow :: Integer -> [Maybe Piece] -> String
writeRow num ((Just piece):xs) =
  let symbol = case piece of
                 (Red,Peasant) -> "r"
                 (Red,Emperor) -> "R"
                 (Black,Peasant) -> "b"
                 (Black,Emperor) -> "B"
  in if num `mod` 2 == 0
    then "   | " ++ symbol ++ " |" ++ (writeRow num xs)
  else
    " " ++ symbol ++ " |   |" ++ (writeRow num xs)

writeRow num ((Nothing):xs) = "   |   |" ++ (writeRow num xs)
writeRow num [] = []

--prettyShow takes a game state and returns a human readable string representing the Gamestate
--NOTE - when testing in ghci, new lines characters will not appear unless you pass the string from prettyShow into putStrLn
--For example, if you want to test the default board in ghci, you should write: putStrLn $ prettyShow defaultBoard
prettyShow :: GameState -> String
prettyShow (player,board) = intercalate "\n" $ reverse $ ["\n", (show player) ++ "'s Turn", "\n", "  ---------------------------------"] ++ (aux 1 (reverse board)) ++ ["\n"]
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

--in the game, x range 0-3 and y range 0-7
--typing move, x range 1-8 and y range 1-8
--when playing the actual game, the coordinates the player puts in 
--will be put through this function to tranform them into "game coordinates"
--every function should use these "game coordintes"
--only the visual component will use the board coordinates
boardCoordsToGameCoords :: (Coordinate,Coordinate) -> (Coordinate,Coordinate)
boardCoordsToGameCoords ((x1,y1),(x2,y2)) =
  let x1GameCoord = xBoardCoordToGameCoord (x1,y1)
      x2GameCoord = xBoardCoordToGameCoord (x2,y2)
  in ((x1GameCoord,y1-1),(x2GameCoord,y2-1))
  where xBoardCoordToGameCoord :: Coordinate -> Int
        xBoardCoordToGameCoord (x,y) = if even y
                                       then (x-2)`div`2
                                       else (x-1)`div`2

boardMoveToGameMove :: Move -> Move
boardMoveToGameMove = map boardCoordsToGameCoords

--function to index a location with a GameState and Coordinate and return a Maybe Piece
getPieceAtIndex :: GameState -> Coordinate -> Maybe Piece
getPieceAtIndex (player,board) (x,y) = head $ drop x (head $ drop (7 - y) board)

isValidMove :: GameState -> Move -> Bool
isValidMove gs [] = False
isValidMove gs [m] = isValidMovement gs m
isValidMove gs (m:ms) = isValidMovement gs m && isValidMove (makeLegalMove gs [m]) ms


isValidMovement :: GameState -> ((Int, Int), (Int, Int)) -> Bool
isValidMovement gs@(Red,board) ((x1,y1),(x2,y2)) =
  let dy = y2-y1
      dx = abs(x1-x2)
  in  x1 `elem` [0..3] && x2 `elem` [0..3] && y1 `elem` [0..7] && y2 `elem` [0..7] && isNothing (getPieceAtIndex gs (x2,y2)) && case getPieceAtIndex gs (x1,y1) of
        Just(Red,Peasant) -> case dy of
                              1 -> dx `elem` [0,1]
                              2 -> dx == 1 && getPieceAtIndex gs (if x1>x2 then x2 else x1,y2-1) `elem` [Just(Black,Peasant),Just(Black,Emperor)]
                              _ -> False
        Just(Red,Emperor) -> case dy of
                              1 -> dx `elem` [0,1]
                              -1 -> dx `elem` [0,1]
                              2 -> dx == 1 && getPieceAtIndex gs (if x1>x2 then x2 else x1,y2-1) `elem` [Just(Black,Peasant),Just(Black,Emperor)]
                              -2 -> dx == 1 && getPieceAtIndex gs (if x2>x1 then x2 else x1,y2+1) `elem` [Just(Black,Peasant),Just(Black,Emperor)]
                              _ -> False
        _ -> False
isValidMovement gs@(Black,board) ((x1,y1),(x2,y2)) =
  let dy = y1-y2
      dx = abs(x1-x2)
  in  x1 `elem` [0..3] && x2 `elem` [0..3] && y1 `elem` [0..7] && y2 `elem` [0..7] && isNothing (getPieceAtIndex gs (x2,y2)) && case getPieceAtIndex gs (x1,y1) of
        Just(Black,Peasant) -> case dy of
                            1 -> dx `elem` [0,1]
                            2 -> dx == 1 && getPieceAtIndex gs (if x1>x2 then x2 else x1,y2-1) `elem` [Just(Red,Peasant),Just(Red,Emperor)]
                            _ -> False
        Just(Black,Emperor) -> case dy of
                            1 -> dx `elem` [0,1]
                            -1 -> dx `elem` [0,1]
                            2 -> dx == 1 && getPieceAtIndex gs (if x1>x2 then x2 else x1,y2-1) `elem` [Just(Red,Peasant),Just(Red,Emperor)]
                            -2 -> dx == 1 && getPieceAtIndex gs (if x2>x1 then x2 else x1,y2+1) `elem` [Just(Red,Peasant),Just(Red,Emperor)]
                            _ -> False
        _ -> False


getValidMoves :: GameState -> [Move]
getValidMoves gs@(Red,board) = [getMovesForPiece gs (x,y) | y <- [0..7], x <- [0..3], getPieceAtIndex gs (x,y) `elem` [Just(Red,Peasant), Just(Red,Emperor)]]
getValidMoves gs@(Black,board) = [getMovesForPiece gs (x,y) | y <- [0..7], x <- [0..3], getPieceAtIndex gs (x,y) `elem` [Just(Black,Peasant), Just(Black,Emperor)]]

getMovesForPiece :: GameState -> Coordinate -> Move
getMovesForPiece gs (x,y) = undefined --filter isValidMove []

f :: Char -> Maybe Piece
f 'n' = Nothing
f 'r' = Just(Red,Peasant)
f 'b' = Just(Black,Peasant)

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

    1   2   3   4   5   6   7   8
  ---------------------------------
8 |   | b |   | b |   | b |   | b |
  ---------------------------------
7 | b |   | b |   | b |   | b |   |
  ---------------------------------
6 |   | b |   | b |   | b |   | b |
  ---------------------------------
5 |   |   |   |   |   |   |   |   |
  ---------------------------------
4 |   |   |   |   |   |   |   |   |
  ---------------------------------
3 | r |   | r |   | r |   | r |   |
  ---------------------------------
2 |   | r |   | r |   | r |   | r |
  ---------------------------------
1 | r |   | r |   | r |   | r |   |
  ---------------------------------

-}
