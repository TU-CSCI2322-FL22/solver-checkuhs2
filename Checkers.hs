module Checkers where

import Data.List
import Data.Maybe (isNothing, isJust)

maxTurns :: Int
maxTurns = 50

data Outcome = Winner Player | Tie
data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasant deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [(Coordinate,Piece)]
type Coordinate = (Int,Int)
type Move = [(Coordinate,Coordinate)]
type Turn = Int
type GameState = (Player,Board,Turn)

prettyShow :: GameState -> String
prettyShow gs@(player,board,turn) = intercalate "\n" $ firstLines ++ [showRow y | y <- [8,7..1]]
  where firstLines = ["", "Turn: " ++ playerString player, "Turn Number: " ++ (show turn),"","    1   2   3   4   5   6   7   8","  ---------------------------------"]
        playerString Black = "Black"
        playerString Red = "Red"
        showRow :: Int -> String
        showRow num = (show num) ++ " | " ++ (intercalate " | " (map (coordinateToString gs) (zip [1..8] (repeat num)))) ++ " |\n  ---------------------------------"

coordinateToString :: GameState -> Coordinate -> String
coordinateToString gs coor = 
  case getPieceAtLocation gs coor of
    Just (Red,Emperor) -> "R"
    Just (Red,Peasant) -> "r"
    Just (Black,Emperor) -> "B"
    Just (Black,Peasant) -> "b"
    Nothing -> " "


uglyShow :: GameState -> [String]
uglyShow gs@(player,board,turn) =
  let fstLine = show turn ++ " " ++ playerToString player
  in fstLine:[rowToString y | y <- [8,7..1]]
  where playerToString Black = "Black"
        playerToString Red = "Red"
        rowToString :: Int -> String
        rowToString y = concatMap pieceToString (zip [1..8] (repeat y))
        pieceToString :: Coordinate -> String
        pieceToString coord =
          case getPieceAtLocation gs coord of
            Just(Red,Peasant) -> "r "
            Just(Red,Emperor) -> "R "
            Just(Black,Peasant) -> "b "
            Just(Black,Emperor) -> "B "
            Nothing -> "_ "

printUglyShow :: GameState -> IO ()
printUglyShow gs = mapM_ putStrLn (uglyShow gs)

getPieceAtLocation :: GameState -> Coordinate -> Maybe Piece
getPieceAtLocation (player,bd,_) coord = lookup coord bd


--checkWinner happens at the start of a "turn"
--takes the current gamestate and the moves the current player can make
--this means that if no moves can be made the 'other' player wins 
checkWinner :: GameState -> Maybe Outcome
checkWinner gs@(player,_,turn) =
  let moves = getValidMoves gs
  in if turn <= maxTurns then Nothing 
  else if null moves then Just $ Winner (getOpponent player) else Just Tie

makeMove :: GameState -> Move -> Maybe GameState
makeMove gs = foldl makePartialMove (Just gs)
  where makePartialMove :: Maybe GameState -> (Coordinate,Coordinate) -> Maybe GameState
        makePartialMove game m =
          case game of Nothing -> Nothing
                       Just g -> if isValidMovement g m
                                 then Just (makeLegalMove g [m])
                                 else Nothing



makeLegalMove :: GameState -> Move -> GameState
makeLegalMove gs@(player,board,turn) [] = (getOpponent player,board,turn+1)
makeLegalMove gs@(player,board,turn) (m:ms) = makeLegalMove (player, changeBoard board m,turn) ms
  where changeBoard :: Board -> (Coordinate,Coordinate) -> Board
        changeBoard bd (s@(_,sRow), e@(_,eRow))
          | abs (sRow-eRow) == 2 =
              let jumpedCoords = getJumpedCoordinates (s,e)
                  piece = getPieceAtLocation gs s
                  jumpedBoard = updateBoard (jumpedCoords,Nothing) $ updateBoard (s,Nothing) bd
              in updateBoard (e,piece) jumpedBoard
          | otherwise =
              let piece = getPieceAtLocation gs s
                  in updateBoard (e,piece) $ updateBoard (s,Nothing) bd

updateBoard :: (Coordinate, Maybe Piece) -> Board -> Board
updateBoard (coord,Nothing) bd = deleteFromBoard coord
  where deleteFromBoard crd = filter (\x -> fst x /= crd) bd
updateBoard (coord,Just piece) bd = (coord,piece):bd

getJumpedCoordinates :: (Coordinate, Coordinate) -> Coordinate
getJumpedCoordinates ((x1,y1),(x2,y2)) = ((x1+x2)`div`2,(y1+y2)`div`2)

getOpponent :: Player -> Player
getOpponent Red = Black
getOpponent Black = Red

isValidMove :: GameState -> Move -> Bool
isValidMove gs [] = False
isValidMove gs [m] = isValidMovement gs m
isValidMove gs (m:ms) = isValidMovement gs m && isValidMove (makeLegalMove gs [m]) ms

getPeasantDirs :: Player -> [Coordinate]
getPeasantDirs Black = [(-1,-1),(1,-1)]
getPeasantDirs Red   = [(-1,1),(1,1)]

getPeasantJumps :: Player -> [Coordinate]
getPeasantJumps Black = [(-2,-2),(2,-2)]
getPeasantJumps Red   = [(-2,2),(2,2)]

getDirs :: [Coordinate]
getDirs = [(-1,-1),(1,-1),(-1,1),(1,1)]

getJumps :: [Coordinate]
getJumps = [(-2,-2),(2,-2),(-2,2),(2,2)]

addCoords :: Coordinate -> Coordinate -> Coordinate
addCoords (x1,y1) (x2,y2) = (x1+x2,y1+y2)

isValidMovement :: GameState -> (Coordinate,Coordinate) -> Bool
isValidMovement gs@(player,board,_) (s@(x1,y1), e@(x2,y2)) =
  let dy = abs (y1-y2)
      piece = getPieceAtLocation gs s
  in checkValidPiece piece && checkBounds &&
     case piece of
      Just (_,Peasant) -> case dy of 1 -> e `elem` map (addCoords s) (getPeasantDirs player)
                                     2 -> e `elem` map (addCoords s) (getPeasantJumps player) && checkJumpedPieceEnemy
                                     _ -> False
      Just (_,Emperor) -> case dy of 1 -> e `elem` map (addCoords s) getDirs
                                     2 -> e `elem` map (addCoords s) getJumps && checkJumpedPieceEnemy
                                     _ -> False
  where checkValidPiece :: Maybe Piece -> Bool
        checkValidPiece piece =
          case piece of Nothing -> False
                        Just(piecePlayer,kind) -> piecePlayer == player
        checkBounds = x2 `elem` [1..8] &&
                      y2 `elem` [1..8] &&
                      isNothing (getPieceAtLocation gs e)
        checkJumpedPieceEnemy = case getPieceAtLocation gs (getJumpedCoordinates (s,e)) of
                                  Just (piecePlayer,kind) -> piecePlayer == getOpponent player
                                  Nothing -> False

getValidMoves :: GameState -> [Move]
getValidMoves gs@(player,board,_) = concatMap getMovesForCell board
  where getMovesForCell (coords,(piecePlayer,_)) = if piecePlayer == player
                                                   then getMovesForPiece gs coords
                                                   else []

getMovesForPiece :: GameState -> Coordinate -> [Move]
getMovesForPiece gs coords@(x,y) = getSingleMoves coords ++ getJumpMoves gs coords
  where getSingleMoves :: Coordinate -> [Move]
        getSingleMoves s@(x1,y1) =
          let possibleSingleMoves = map (addCoords s) getDirs
          in [ [(s,e)] | e <- possibleSingleMoves, isValidMovement gs (s,e)]
        getJumpMoves :: GameState -> Coordinate -> [Move]
        getJumpMoves state s@(x1,y1) =
          let possibleJumpLocations = filter (\e -> isValidMovement state (s,e)) $ map (addCoords s) getJumps
              followUp = [getJumpMoves (makeLegalMove state [(s,e)]) e | e <- possibleJumpLocations]
              firstMoves = map (\e -> [(s,e)]) possibleJumpLocations
          in firstMoves ++ [head m : fm | m@((s1,e1):ms) <- firstMoves, f <- followUp, fm@((s2,e2):fms) <- f, s2 == e1]

<<<<<<< HEAD
getPieceFromChar :: Char -> Maybe Piece
getPieceFromChar 'R' = Just (Red,Emperor)
getPieceFromChar 'r' = Just (Red,Peasant)
getPieceFromChar 'B' = Just (Black,Emperor)
getPieceFromChar 'b' = Just (Black,Peasant)
getPieceFromChar _ = Nothing
=======
>>>>>>> abb79b5d849044954eba1d74acb857d340fea736

makeRow :: Int -> String -> Board
makeRow y pieces
  | even y =
    concatMap makePiece (zip pieces [2,4,6,8])
  | otherwise =
    concatMap makePiece (zip pieces [1,3,5,7])
    where makePiece :: (Char,Int) -> [(Coordinate,Piece)]
          makePiece (char,x) = 
            case getPieceFromChar char of
              Nothing -> []
              Just piece -> [((x,y),piece)]


defaultBoard =
  makeRow 8 "bbbb" ++
  makeRow 7 "bbbb" ++
  makeRow 6 "bbbb" ++
  --middle row
  --middle row
  makeRow 3 "rrrr" ++
  makeRow 2 "rrrr" ++
  makeRow 1 "rrrr"

defaultGame :: GameState
defaultGame = (Black,defaultBoard,0)

testBoard1 =
  makeRow 5 "nnbn" ++
  makeRow 4 "nrnn" 

num :: Turn
num = 1

testGame1B = (Black,testBoard1,num)
testGame1R = (Red,testBoard1,num)

testBoard2 =
  makeRow 8 "nnnb" ++
  makeRow 7 "nnnr" ++
  makeRow 5 "nnrr" ++
  --middle row
  --middle row
  makeRow 3 "nrrr" 

testGame2 = (Black,testBoard2)


{-
|||||(2,8)|||||(4,8)|||||(6,8)|||||(8,8)
(1,7)|||||(3,7)|||||(5,7)|||||(7,7)|||||
|||||(2,6)|||||(4,6)|||||(6,6)|||||(8,6)
(1,5)|||||(3,5)|||||(5,5)|||||(7,5)|||||
|||||(2,4)|||||(4,4)|||||(6,4)|||||(8,4)
(1,3)|||||(3,3)|||||(5,3)|||||(7,3)|||||
|||||(2,2)|||||(4,2)|||||(6,2)|||||(8,2)
(1,1)|||||(3,1)|||||(5,1)|||||(7,1)|||||


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
