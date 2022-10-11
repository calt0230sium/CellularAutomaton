module Plan ( 
   runAutomaton,
   LifeState (Born, Alive, Dead),
) where 

import System.IO
import Debug.Trace
import Control.Concurrent
import System.Process
import System.Info


-- Grid cellular automaton --

data LifeState = Born | Alive | Dead

twoDimToOneDim :: (Int, Int) -> Int  -> Int
twoDimToOneDim (i, j) automaton_size = (((mod j automaton_size) * automaton_size) + (mod i automaton_size))

neighborsValue :: Int -> [Int] -> [Int] -> [Int]
neighborsValue element_id neighbors_id previousState = case neighbors_id of
  x : rest -> (if (x /= element_id) then (previousState!!x) else 0) : (neighborsValue element_id rest previousState)
  [] -> []
  
verifyRule :: Int -> [Int] -> [LifeState] -> Int
verifyRule element_value neighbors_value rule = case (rule!!(sum neighbors_value)) of
  Born -> 1
  Alive -> element_value
  Dead -> 0

computeNextElementState :: Int -> Int -> Int -> [LifeState] -> [Int] -> Int
computeNextElementState automaton_size element_value element_id rule totalState = do
  let i = (mod (element_id) automaton_size)
  let j = (div (element_id) automaton_size)

  let neighbors_id = map (`twoDimToOneDim` automaton_size) [(a,b) | a <- [i-1..i+1], b <- [j-1..j+1]]
  let neighbors_value = (neighborsValue element_id neighbors_id totalState)

  (verifyRule element_value neighbors_value rule)

automaton :: Int -> [LifeState] -> [Int] -> [Int] -> [Int]
automaton automaton_size rule totalState previousState = case previousState of
  x : rest -> (computeNextElementState automaton_size x ((length totalState) - (length previousState)) rule totalState) : (automaton automaton_size rule totalState rest)
  otherwise -> []

stateToString :: Int -> [Int] -> String
stateToString automaton_size state = case state of
  0 : rest -> (if (mod (length rest) automaton_size) == 0 then " \n" else "  ") ++ (stateToString automaton_size rest)
  1 : rest -> (if (mod (length rest) automaton_size) == 0 then "■\n" else "■ ") ++ (stateToString automaton_size rest)
  _ -> ""

-- IO functions, side effect --

clearTerminal
  | os == "mingw32" = callCommand "cls"
  | otherwise = callCommand "clear"

runAutomaton automaton_size state rule 0 = return()
runAutomaton automaton_size state rule n = do
  putStrLn (stateToString automaton_size state)
  threadDelay 100000
  clearTerminal
  runAutomaton automaton_size (automaton automaton_size rule state state) rule (n-1)