module Main where

import System.Directory
import System.IO
import System.Environment
import Text.Read (readMaybe)
import Data.Char
import System.Console.GetOpt
import System.Console.GetOpt

import Checkers
import InputOutput
import Solver
import Data.Maybe

data Flag = FWinner | Depth String | Help | Mover String | Verbose | Interactive deriving (Eq,Show)

options :: [OptDescr Flag]
options = [  Option ['w'] ["winner"] (NoArg FWinner) "Print out the best move, using an exhaustive search (no cut-off depth)."
          ,  Option ['d'] ["depth"] (ReqArg Depth "#") "Use this number as a cutoff depth, instead of your default."
          ,  Option ['h'] ["help"] (NoArg Help) "Prints out a help message and quits the program."
          ,  Option ['m'] ["move"] (ReqArg Mover "#") "Makes <move> and prints out the resulting board."
          ,  Option ['v'] ["verbose"] (NoArg Verbose) "Outputs both the move and a description of how good it is: win, lose, tie, or a rating."
          ]

getDepth :: [Flag] -> (Bool,Int)
getDepth [] = (False,3)
getDepth ((Depth x):_) =
    case readMaybe x of
        Nothing -> error "Invalid input to depth flag"
        Just depth -> (True,depth)
getDepth (_:flags) = getDepth flags

getMove :: [Flag] -> Maybe Move
getMove ((Mover x):_) = parseMove x
getMove (_:xs) = getMove xs
getMove [] = Nothing

main = do
    args <- getArgs
    let (flags,inputs,error) = getOpt Permute options args
    if Help `elem` flags || not (null error)
    then putStrLn $ usageInfo "Usage: [options] [file]" options
    else do let fname = if null inputs then "defaultBoard.txt" else head inputs
            gs <- loadGame fname
            chooseAction flags gs
    


chooseAction :: [Flag] -> GameState -> IO ()
chooseAction flags gs
 | isJust move = case move of
                    Nothing -> error "how did we get here? hmmmmm"
                    Just move -> do case makeMove gs move of 
                                      Nothing -> error "Invalid Move"
                                      Just validGs -> do printUglyShow validGs 
                                                         if Verbose `elem` flags then verboseOutput gs move
                                                         else return ()
 | FWinner `elem` flags = if fst (getDepth flags) then error "flags winner and depth are mutually exclusive"
                          else printOutput gs (-1) (Verbose `elem` flags)
 | otherwise = printOutput gs (snd (getDepth flags)) (Verbose `elem` flags)
    where move = getMove flags

printOutput :: GameState -> Int -> Bool -> IO ()
printOutput gs depth isVerbose = 
    let move = depthBestMove depth gs
    in do print move
          if isVerbose then verboseOutput gs move
          else return ()
          
        

verboseOutput :: GameState -> Move -> IO ()
verboseOutput gs@(player,board,turn) move = 
    let movedState = makeLegalMove gs move
        prevScore = rateGameState gs
        score = rateGameState movedState - prevScore
        string = "This move " ++ if score >= 1000 then "is a Winning Move" else
                         if score <= -1000 then "is a losing Move" else 
                         if turn == 0 then "will lead to a tie" 
                         else "has a rating of " ++ show score
    in putStrLn string

--parseMove takes a string and returns a Just Move if the string is a properly formatted Move,
--otherwise, it returns Nothing
parseMove :: String -> Maybe Move
parseMove [] = Nothing
parseMove (firstChar:strTail) =
  let 
    parse :: String -> Bool -> Move -> Maybe Move
    parse [] checkComma move =  Nothing
    parse str False move
      | length str < 13 = Nothing
      | isValidMoveString (take 13 str) = parse (drop 13 str) True ((stringToMove (take 13 str)) ++ move)
      | otherwise = Nothing
    parse (nextChar:str) True move 
      | nextChar == ',' = parse str False move
      | nextChar == ']' = Just $ reverse move
      | otherwise = Nothing
  in
    if firstChar == '['
      then parse strTail False []
    else 
      Nothing

--validNum is a helper function for isValidMoveString that checks if a char is a valid x/y location
validNum :: Char -> Bool
validNum c = c `elem` "12345678"

--charToInt converts Char to Ints, shocking
--There is most likely a builtin function for this, but I could not find it
charToInt :: Char -> Int
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt '0' = 0

--This function takes a String and returns True if the String is a properly formatted single move
isValidMoveString :: String -> Bool
isValidMoveString str =
  length str == 13 &&
  str !! 0 == '(' &&
  str !! 1 == '(' &&
  validNum (str !! 2) &&
  str !! 3 == ',' &&
  validNum (str !! 4) &&
  str !! 5 == ')' &&
  str !! 6 == ',' &&
  str !! 7 == '(' &&
  validNum (str !! 8) &&
  str !! 9 == ',' &&
  validNum (str !! 10) &&
  str !! 11 == ')' &&
  str !! 12 == ')' 

--This funciton takes a String and converts it to a single element Move
--Note that this function should only be used when you are certain the string that you are passing
--in is properly formatted
stringToMove :: String -> Move
stringToMove str = 
  let
    firstCoor = (charToInt (str !! 2), charToInt (str !! 4))
    secondCoor = (charToInt (str !! 8), charToInt (str !! 10))
  in [(firstCoor,secondCoor)]



