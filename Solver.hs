module Solver where

import Checkers
import Debug.Trace
import Data.Maybe (catMaybes)

--This is the previouis implementation of whoWillWin that may or may not be any more efficient
--Function "who will win" that takes a Game and returns an Outcome. 
--Considers every valid move, the resulting game state, 
--and chooses the move with the best outcome for the current player.
predictedWinner :: GameState -> Outcome
predictedWinner gs@(player,board,turn) = 
  case nodeOutcome of
    Just outcome -> outcome
    Nothing -> error "Game does not end in a winner or tie"
  where possibleMoves = [makeLegalMove gs move | move <- (getValidMoves gs)]
        
        checkMoves :: Maybe Outcome -> GameState -> Maybe Outcome
        checkMoves Nothing next = Just $ predictedWinner next
        checkMoves (Just prevOutcome) next =
          case prevOutcome of 
            Winner p -> 
              if p == player then Just $ Winner player
              else Just $ predictedWinner next
            Tie -> 
              let nextOutcome = predictedWinner next
              in case nextOutcome of 
                Winner player -> Just $ Winner player
                _ -> Just Tie
        
        currentOutcomes =  map checkWinner possibleMoves
        nodeOutcome = 
          if (Just (Winner player)) `elem` currentOutcomes
            then Just $ Winner player

          else if null possibleMoves
            then Just $ Winner $ getOpponent player
          else
            foldl checkMoves Nothing possibleMoves

--whoWillWin Function
predictedWinner2 :: GameState -> Outcome
predictedWinner2 gs@(player,board,turn) =
  case checkWinner gs of
    Nothing ->
      let possibleMoves = catMaybes $ [makeMove gs move | move <- (getValidMoves gs)]
          outcomes = map predictedWinner2 possibleMoves
      in if Winner player `elem` outcomes
        then Winner player
      else if Tie `elem` outcomes
        then Tie
      else Winner $ getOpponent player
    Just (Winner p) -> 
      if p == player then Winner player
      else Winner $ getOpponent player
    Just Tie -> Tie

          
--Function "best move" that takes a Game and return the best Move.
--Given a game state, search for a move that can force a win for the current player. 
--Failing that, return a move that can force a tie for the current player. 
--This will involve recursively searching through the game states that result from that move. 
bestMove :: GameState -> (GameState -> Outcome) -> Move
bestMove gs@(player,board,turn) func =
    let moves = getValidMoves gs
        lst = [(func (makeLegalMove gs m),m) | m <- moves]
        wins = [move | (w,move) <- lst, w == Winner player]
        ties = [move | (w,move) <- lst, w == Tie]
    in  if null wins
        then    if  null ties
                then head moves
                else head ties
        else head wins


--Takes a GameState and returns an integer representing the current player's chance of winning where
--a positive number is a game in their favor and a negative is in the opponents favor
rateGameState :: GameState -> Int
rateGameState gs@(player,board,turn) = 
  let 
    calcScores :: Board -> (Int,Int) -> (Int,Int)
    calcScores [] (redScore,blackScore) = (redScore,blackScore)
    calcScores ((piece@((x,y),(p,k))):ps) (redScore,blackScore) = 
      let cornerBonus = if (x == 1 || x == 8 || y == 1 || y == 8) then 1 else 0
          kindScore = 
            case k of
              Peasant -> 1
              Emperor -> 5
      in
        case p of
          Red -> calcScores ps (redScore + kindScore + cornerBonus, blackScore)
          Black -> calcScores ps (redScore, blackScore + kindScore + cornerBonus)
    (redTotal,blackTotal) = calcScores board (0,0)
  in
   case player of
     Red -> redTotal - blackTotal
     Black -> blackTotal - redTotal

