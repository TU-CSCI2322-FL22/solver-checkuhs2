module Main where

import System.Directory
import System.IO
import System.Console.GetOpt

import Checkers
import InputOutput
import Solver

data Flag = Winner | Depth String | Help | Mover String | Verbose | Interactive deriving (Eq,Show)

options :: [OptDescr String]
options = [ Option ['w'] ["winner"] (NoArg Winner) "Print out the best move, using an exhaustive search (no cut-off depth)."
           ,Option ['d'] ["depth"] (RegArg Depth "#") "Use this number as a cutoff depth, instead of your default."
           ,Option ['h'] ["help"] (NoArg Help)
           ,Option ['m'] ["move"] (RegArg Mover "#")
          ]

getMove :: [Flag] -> Maybe Move
getMove ((Mover x):_) = Nothing --TODO
getMove (_:xs) = getMove xs
getMove [] = Nothing

parseMove :: String -> Maybe Move
parseMove = undefined

main = do
    --args <- getArgs **will be used**
    let (flags,inputs,error) = getOpt Permute options args
    if Help `elem` flags || (not $ null error)
    then putStrLn $ usageInfo "Usage: "

getBoardFromFile :: IO GameState
getBoardFromFile = do
    putStr $ "Board File: "
    hFlush stdout
    maybeFile <- getLine
    isValidFile <- doesFileExist maybeFile
    if isValidFile 
    then loadGame maybeFile
    else getBoardFromFile