module Main where

import Checkers
import InputOutput
import Solver

main = do
    gs <- loadGame "text.txt"
    printUglyShow gs

--Ideas for parse move
--1. Filter all non-number characters then use pairs of those numbers to construct coordinates

parseMove :: String -> Maybe Move
parseMove str = 
