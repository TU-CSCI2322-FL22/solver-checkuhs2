module Main where

import System.Directory
import System.IO

import Checkers
import InputOutput
import Solver

main = do
    --args <- getArgs **will be used**
    gs <- getBoardFromFile
    printUglyShow gs

getBoardFromFile :: IO GameState
getBoardFromFile = do
    putStr $ "Board File: "
    hFlush stdout
    maybeFile <- getLine
    isValidFile <- doesFileExist maybeFile
    if isValidFile 
    then loadGame maybeFile
    else getBoardFromFile