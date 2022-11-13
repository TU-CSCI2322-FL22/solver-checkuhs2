module InputOutput where

import System.IO
import Checkers
import Data.Map (mapMaybe)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

readGame :: String -> Maybe GameState
readGame str =
    let (firstLine:board) = lines str
        gameData = getGameStateData $ words firstLine
    in case gameData of
        Nothing           -> Nothing
        Just(turn,player) -> case readMaybeBoard board of
                                Nothing -> Nothing
                                Just board -> Just (player,board,turn)
    where getGameStateData :: [String] -> Maybe (Int,Player)
          getGameStateData fstLine =
            if length fstLine /= 2 then Nothing
            else case readMaybe (head fstLine) :: Maybe Int of
                    Nothing -> Nothing
                    Just i  -> case readMaybePlayer (last fstLine) of
                                Nothing -> Nothing
                                Just player -> Just (i,player)
          readMaybePlayer :: String -> Maybe Player
          readMaybePlayer "Black" = Just Black
          readMaybePlayer "Red" = Just Red
          readMaybePlayer _ = Nothing
          readMaybeBoard :: [String] -> Maybe Board
          readMaybeBoard boardStrs =
            let rowStrs = zip rows boardStrs
                validRows = catMaybes $ map readMaybeRow rowStrs
            in if length validRows == 8 then Just (concat validRows) else Nothing
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
            then case getPieceFromChar char of
                    Nothing -> Nothing
                    Just piece -> Just(coord,piece)
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

putWinner :: GameState -> IO ()
putWinner = undefined