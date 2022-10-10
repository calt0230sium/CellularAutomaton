module Line ( 
   runAutomaton,
) where 


binaryToDecimal :: [Int] -> Int
binaryToDecimal binary = case binary of
  0 : rest -> binaryToDecimal rest
  1 : rest -> 2 ^ (length rest) + (binaryToDecimal rest)
  _ -> 0

-- Elementary cellular automaton --

automaton :: Int -> Int -> Int -> [Int] -> Int
automaton nl element nr rule = (rule!!(7 - (binaryToDecimal [nl, element, nr])))

nextState :: [Int] -> [Int] -> [Int]
nextState currentState rule = case currentState of
  -- if 2 neighbors
  nl : element : nr : rest -> (automaton nl element nr rule) : (nextState ([element] ++ [nr] ++ rest) rule)
  -- if 1 neighbor
  [nl, element] -> (automaton nl element 0 rule) : []
  -- if 0 neighbor
  _ -> []

initAutomaton :: [Int] -> [Int] -> [Int]
initAutomaton currentState rule = [(automaton 0 (currentState!!0) (currentState!!1) rule)] ++ (nextState currentState rule)

stateToString :: [Int] -> String
stateToString state = case state of
  0 : rest -> " " ++ (stateToString rest)
  1 : rest -> "■" ++ (stateToString rest)
  _ -> ""

-- IO functions, side effect --

runAutomaton state rule 0 = return()
runAutomaton state rule n = do
  putStrLn (stateToString state)
  runAutomaton (initAutomaton state rule) rule (n-1)
