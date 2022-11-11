module InputOutput where

import Checkers

readGame :: String -> Game
readGame = undefined

showGame :: Game -> String
showGame = undefined

writeGame :: Game -> FilePath -> IO ()
writeGame = undefined

loadGame :: FilePath -> IO Game 
loadGame = undefined

putWinner :: Game -> IO ()
putWinner = undefined