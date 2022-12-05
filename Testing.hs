module Testing where

import Checkers
import Solver
import InputOutput

oneLeft :: Turn
oneLeft = 1

fiveLeft :: Turn
fiveLeft = 5

testBoard1 =
  makeRow 5 "nnbn" ++
  makeRow 4 "nrnn"



testGame1B = (Black,testBoard1,oneLeft)
testGame1R = (Red,testBoard1,oneLeft)

testBoard2 =
  makeRow 8 "nnnb" ++
  makeRow 7 "nnnr" ++
  makeRow 5 "nnrr" ++
  makeRow 3 "nrrr" 

testGame2 = (Black,testBoard2,fiveLeft)
testGame2R = (Red,testBoard2,fiveLeft)


testBoard3 =
  makeRow 7 "nnnB" ++
  makeRow 5 "nnnR"

testGame3 = (Red,testBoard3,fiveLeft)

testBoard4 =
  makeRow 8 "RbRb" ++
  makeRow 7 "bbbb" ++
  makeRow 2 "rrrr" ++
  makeRow 1 "BrBr"

testGame4 = (Red,testBoard4,fiveLeft)
testGame4B = (Black,testBoard4,fiveLeft)

testBoard5 = 
  makeRow 7 "nnnB" ++
  makeRow 5 "nnRR"

testGame5 = (Black,testBoard5,fiveLeft)
testGame5R = (Red,testBoard5,fiveLeft)
