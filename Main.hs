module Main where

import Checkers
import InputOutput
import Solver
import Data.List

main = do
    gs <- loadGame "text.txt"
    printUglyShow gs


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


