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
getMove ((Mover x):_) = Nothing --TODO
getMove (_:xs) = getMove xs
getMove [] = Nothing

parseMove :: String -> Maybe Move
parseMove = undefined

main = do
    args <- getArgs
    let (flags,inputs,error) = getOpt Permute options args
    let fname = if null inputs then "defaultFilePath" else head inputs
    gs <- loadGame fname
    if Help `elem` flags || not (null error)
    then putStrLn $ usageInfo "Usage: [options] [file]" options
    else chooseAction flags gs


chooseAction :: [Flag] -> GameState -> IO ()
chooseAction flags gs
 | let move = getMove flags, isJust move = if length flags /= 1
                                           then error "move flag mutually exclusive with every other flag"
                                           else case move of
                                               Nothing -> error "how did we get here? hmmmmm"
                                               Just move -> printUglyShow (makeLegalMove gs move)
 | FWinner `elem` flags = if fst (getDepth flags) then error "flags winner and depth are mutually exclusive"
                          else printOutput gs (-1) (Verbose `elem` flags)
 | otherwise = printOutput gs (snd (getDepth flags)) (Verbose `elem` flags)

printOutput :: GameState -> Int -> Bool -> IO ()
printOutput gs depth isVerbose = 
    let move = bestMove gs (whoMightWin depth) 
    in do print move
          if isVerbose then verboseOutput gs move
          else return ()
          
        

verboseOutput :: GameState -> Move -> IO ()
verboseOutput gs@(player,board,turn) move = 
    let movedState = makeLegalMove gs move
        score = -1 * (rateGameState movedState) --multiplied by -1 because we want the score for the person who made the previous move
        string = ": " ++ if score >= {- winningscore -} then "Win" else
                         if score <= {- losingscore -} then "Lose" else 
                         if turn == 0 then "Tie" 
                         else "Rating of " ++ show score
    in print string

    

