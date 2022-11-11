module Checkers where

import Data.List
import Data.Maybe (isNothing, isJust)


data Outcome = Player | Tie
data Player = Red | Black deriving (Eq,Show)
data Kind = Emperor | Peasant deriving (Eq,Show)

type Piece = (Player,Kind)
type Board = [(Coordinate,Piece)]
type Coordinate = (Int,Int)
type Move = [(Coordinate,Coordinate)]
type Turn = Int
type GameState = (Player,Board,Turn)

{-
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
-}
prettyShow :: GameState -> String
prettyShow = undefined

uglyShow :: GameState -> [String]
uglyShow gs@(player,board,turn) =
  let fstLine = playerToString player
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

printUglyShow gs = mapM_ putStrLn (uglyShow gs)

getPieceAtLocation :: GameState -> Coordinate -> Maybe Piece
getPieceAtLocation (player,bd) coord = lookup coord bd


--checkWinner happens at the start of a "turn"
--takes the current gamestate and the moves the current player can make
--this means that if no moves can be made the 'other' player wins 
checkWinner :: GameState -> Maybe Outcome
checkWinner gs@(player,_) =
  let moves = getValidMoves gs
  in if null moves then Just (getOpponent player) else Nothing

makeMove :: GameState -> Move -> Maybe GameState
makeMove gs = foldl makePartialMove (Just gs)
  where makePartialMove :: Maybe GameState -> (Coordinate,Coordinate) -> Maybe GameState
        makePartialMove game m =
          case game of Nothing -> Nothing
                       Just g -> if isValidMovement g m
                                 then Just (makeLegalMove g [m])
                                 else Nothing



makeLegalMove :: GameState -> Move -> GameState
makeLegalMove gs@(player,board,turn) [] = (player,board,turn+1)
makeLegalMove gs@(player,board,_) (m:ms) = makeLegalMove (player, changeBoard board m) ms
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
isValidMovement gs@(player,board) (s@(x1,y1), e@(x2,y2)) =
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
getValidMoves gs@(player,board) = concatMap getMovesForCell board
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

{-
getValidMoves :: GameState -> [Move]
getValidMoves gs@(player,board) = getMovesInRows board 7
  where getMovesInRows :: Board -> Int -> [Move]
        getMovesInRows [] _ = []
        getMovesInRows (row:rows) y = getMovesForRow row (0,y) ++ getMovesInRows rows (y-1)
        getMovesForRow :: [Maybe Piece] -> Coordinate -> [Move]
        getMovesForRow [] _ = []
        getMovesForRow (Just(piecePlayer,_):pieces) coord@(x,y) = (if piecePlayer == player then getMovesForPiece coord else []) ++ getMovesForRow pieces (x+1,y)
        getMovesForRow (Nothing:pieces) (x,y) = getMovesForRow pieces (x+1,y)
        getMovesForPiece :: Coordinate -> [Move]
        getMovesForPiece coord = getSingleMoves coord ++ getJumpMoves gs coord
        getSingleMoves :: Coordinate -> [Move]
        getSingleMoves (x,y) = [[((x,y),(x2,y2))] | y2 <- [y-1..y+1], x2 <- [x-1..x+1], isValidMove gs [((x,y),(x2,y2))]]
        getJumpMoves :: GameState -> Coordinate -> [Move]
        getJumpMoves state p1@(x,y) = [[((x,y),(x2,y2))] | p2@(x2,y2) <- possibleJumpLocs (x,y), isValidMove state [((x,y),(x2,y2))]]
        -- getJumpMove :: GameState -> (Coordinate,Coordinate) -> Move
        -- getJumpMove state (start,end) = [(start,end)] : [(start,end)] ++ getJumpMoves (makeLegalMove state [(start,end)]) (end)

possibleJumpLocs :: Coordinate -> [Coordinate]
possibleJumpLocs (x,y) = [((x-1),(y-2)),((x-1),(y+2)),((x+1),(y-2)),((x+1),(y+2))]
       
        The difficulty with getJump move is that every possible jump must be considered
        This includes cases where a string of jumps are jumped with more jumps possible
        Each individual case needs to have its own move, which means the recursion must return
        a list of lists, each for every possible stopping place
        -}







makeRow :: Int -> String -> Board
makeRow y pieces
  | even y =
    concatMap makePiece (zip pieces [2,4,6,8])
  | otherwise =
    concatMap makePiece (zip pieces [1,3,5,7])
    where makePiece :: (Char,Int) -> [(Coordinate,Piece)]
          makePiece ('b',x) = [((x,y),(Black,Peasant))]
          makePiece ('B',x) = [((x,y),(Black,Emperor))]
          makePiece ('r',x) = [((x,y),(Red,Peasant))]
          makePiece ('R',x) = [((x,y),(Red,Emperor))]
          makePiece ('n',x) = []


defaultBoard =
  makeRow 8 "bbbb" ++
  makeRow 7 "bbbb" ++
  makeRow 6 "bbbb" ++
  --middle row
  --middle row
  makeRow 3 "rrrr" ++
  makeRow 2 "rrrr" ++
  makeRow 1 "rrrr"

defaultGame = (Black,defaultBoard)

testBoard1 =
  makeRow 5 "nnbn" ++
  makeRow 4 "nrnn" 

testGame1B = (Black,testBoard1)
testGame1R = (Red,testBoard1)

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
