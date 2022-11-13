module Main where

import Checkers
import InputOutput
import Solver

main = do
    gs <- loadGame "text.txt"
    printUglyShow gs