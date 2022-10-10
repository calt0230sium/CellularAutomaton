import Line
import Plan

import System.Random (Random, RandomGen, newStdGen, randomRs)
import System.IO


-- functions --

getInt :: IO Int
getInt = readLn

decimalToBinary :: Int -> [Int]
decimalToBinary decimal
  | decimal == 0 = [0]
  | otherwise = (decimalToBinary (div decimal 2)) ++ [mod decimal 2] 

binaryToDecimal :: [Int] -> Int
binaryToDecimal binary = case binary of
  0 : rest -> binaryToDecimal rest
  1 : rest -> 2 ^ (length rest) + (binaryToDecimal rest)
  _ -> 0

ruleCorrection :: [Int] -> [Int]
ruleCorrection list
  | (length list) > 8  = case list of
    x : rest -> ruleCorrection rest
    _ -> [] 
  | (length list) == 8 = list 
  | (length list) < 8  = (take (8 - (length list)) (repeat 0)) ++ list

randomList :: (Random a, RandomGen g) => Int -> a -> a -> g -> [a]
randomList len lower higher gen = take len $ randomRs (lower, higher) gen

gliderInit :: [Int]
gliderInit = [ 0,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,1,1,1,0, 0,0,0,0,0 ]

planRuleToString :: [LifeState] -> String
planRuleToString rule = case rule of
  Dead : rest -> "Dead " ++ (planRuleToString rest)
  Born : rest -> "Born " ++ (planRuleToString rest)
  Alive : rest -> "Alive " ++ (planRuleToString rest)
  [] -> ""

-- IO functions, side effect --

lineAutomaton = do
  let sizeList = 40
  let initList = (take sizeList (repeat 0)) ++ [1] ++ (take sizeList (repeat 0))
  
  putStrLn "Enter the rule :"
  
  input <- getInt
  let rule = ruleCorrection (decimalToBinary input)
  
  putStr "Elementary cellular automaton, rule "
  putStrLn (show rule)

  Line.runAutomaton initList rule sizeList

planAutomaton = do
  let rule = [Dead, Dead, Alive, Born, Dead, Dead, Dead, Dead, Dead]
  let size1 = 5
  let size2 = 50

  putStrLn "Choose initial state :"
  putStrLn $ "1. Glider, grid" ++ (show size1) ++ "x" ++ (show size1)
  putStrLn $ "2. Random, grid" ++ (show size2) ++ "x" ++ (show size2)

  input <- getInt
  gen <- newStdGen

  putStr "2D cellular automaton, rule "
  putStrLn (planRuleToString rule)

  case input of
    1 -> Plan.runAutomaton size1 gliderInit rule 30 
    2 -> Plan.runAutomaton size2 (randomList (size2^(2)) 0 1 gen) rule 60
    otherwise -> putStrLn "Error, retry please."

main = do
  -- Choose the automaton
  putStrLn "Choose the dimension of the cellular automaton (1 or 2) :"

  input <- getInt

  case input of
    1 -> lineAutomaton
    2 -> planAutomaton
    otherwise -> putStrLn "Error, retry please."
