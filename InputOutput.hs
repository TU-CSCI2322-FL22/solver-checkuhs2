module InputOutput where

import System.IO
import Checkers
import Solver
import Data.Map (mapMaybe)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

readGame :: String -> Maybe GameState
readGame str =
    case str of 
      [] -> Nothing
      str -> let (firstLine:board) = lines str
             in do (turn,player) <- getGameStateData $ words firstLine
                   bd <- readMaybeBoard board
                   return (player,bd,turn)
    where getGameStateData :: [String] -> Maybe (Int,Player)
          getGameStateData fstLine =
            if length fstLine /= 2 then Nothing
            else do i <- readMaybe (head fstLine)
                    player <- readMaybePlayer (last fstLine)
                    return (i,player)
          readMaybePlayer :: String -> Maybe Player
          readMaybePlayer "Black" = Just Black
          readMaybePlayer "Red" = Just Red
          readMaybePlayer _ = Nothing
          readMaybeBoard :: [String] -> Maybe Board
          readMaybeBoard boardStrs =
            let rowStrs = zip rows boardStrs
            in do validRows <- sequence $ map readMaybeRow rowStrs
                  if length validRows == 8
                  then Just (concat validRows)
                  else Nothing
          readMaybeRow :: (Int,String) -> Maybe Board
          readMaybeRow (y,str) =
            let pieces = concat $ words str --remove whitespace
                coords = zip [1..8] $ repeat y
                coordPieceStringPairs = filter (\(c,s) -> s /= '_') $ zip coords pieces
                maybePieces = map readMaybePiece coordPieceStringPairs
            in if length pieces == 8 then sequence maybePieces else Nothing
          readMaybePiece :: (Coordinate,Char) -> Maybe (Coordinate,Piece)
          readMaybePiece (coord,char) =
            if validCoord coord
            then do piece <- getPieceFromChar char
                    return (coord,piece)
            else Nothing
          validCoord :: Coordinate -> Bool
          validCoord (x,y)
            | even y =
                x `elem` [2,4,6,8]
            | otherwise =
                x `elem` [1,3,5,7]
          rows :: [Int]
          rows = [8,7..1]

showGame :: GameState -> String
showGame gs = unlines $ uglyShow gs

printMove :: Move -> IO ()
printMove move = putStrLn $ show move

printUglyShow :: GameState -> IO ()
printUglyShow gs = mapM_ putStrLn (uglyShow gs)

writeGame :: GameState -> FilePath -> IO ()
writeGame gs@(player,board,turn) path =
  let string = showGame gs
  in do writeFile path string

loadGame :: FilePath -> IO GameState
loadGame path =
       do contents <- readFile path
          case readGame contents of
            Nothing -> error "Couldn't parse gameState from file"
            Just gs -> return gs

--call who will win to IO
putWinner :: GameState -> IO ()
putWinner gs = print (predictedWinner2 gs)