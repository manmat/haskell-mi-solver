module Cups where

import Data.List

type State = [Int]

isValidState :: State -> State -> Bool
isValidState init state = length init == (length state) && foldl (&&) True (zipWith (<=) state init)

updateNth :: Int -> Int -> State -> State
updateNth 0 newValue (c : cs) = newValue : cs
updateNth n newValue (c : cs) = c : (updateNth (n - 1) newValue cs)
updateNth _ _ [] 			  = []

pour :: State -> Int -> Int -> State -> State
pour init from to state = ((updateNth from (state !! from - amount)) . (updateNth to (state !! to + amount))) state where 
	amount = min (state !! from) ((init !! to) - (state !! to))

empty :: Int -> State -> State
empty n = updateNth n 0

fill :: State -> Int -> State -> State
fill init n = updateNth n (init !! n)

data Step = Empty Int | Fill Int | Pour Int Int deriving (Show, Eq)

possibleSteps :: State -> [Step]
possibleSteps init = [Empty n | n <- cups] ++ [Fill n | n <- cups] ++ [Pour n m | n <- cups, m <- cups, n /= m] where 
	cups = [0 .. (length init - 1)]

applyStep :: State -> Step -> State -> State
applyStep _ (Empty n) = empty n
applyStep init (Fill n) = fill init n
applyStep init (Pour n m) = pour init n m

applySteps :: State -> [Step] -> State -> [State]
applySteps _ [] _ = []
applySteps initState (step : steps) state = applyStep initState step state : (applySteps initState steps state)

isSolution :: Int -> State -> Bool
isSolution = elem

type Path = ([Step], State)
type Layer = [Path]

searchForSolutionInLayer :: Int -> Layer -> Maybe Int
searchForSolutionInLayer _ [] = Nothing
searchForSolutionInLayer n layer = findIndex containesSolution layer where 
	containesSolution (xs, state) = isSolution n state

isAlreadyReached :: State -> [Layer] -> Bool
isAlreadyReached _ [] = False
isAlreadyReached state (layer : layers) = (findIndex (\x -> snd x == state) layer) /= Nothing || isAlreadyReached state layers

solveProblem :: Int -> State -> [Step]
solveProblem _ [] = []
solveProblem 0 _ = []
solveProblem n initState = searchSolution n initState [[([], replicate (length initState) 0)]] steps where
	steps = possibleSteps initState

comparePath :: Path -> Path -> Bool
comparePath (_, state1) (_, state2) = state1 == state2

addStepToPath :: State -> Path -> Step -> Path
addStepToPath initState (steps, state) step = (step : steps, applyStep initState step state)

searchSolution :: Int -> State -> [Layer] -> [Step] -> [Step]
searchSolution n initState (layer : layers) steps = case searchForSolutionInLayer n newLayer of
	Nothing -> if (newLayer == []) then [] else searchSolution n initState (newLayer : (layer : layers)) steps
	(Just x) -> reverse (fst (newLayer !! x))
	where
		newLayer = nubBy comparePath [addStepToPath initState path step | path <- layer, step <- steps, not (isAlreadyReached (applyStep initState step (snd path)) (layer : layers))]

prettyPrintStep :: Step -> String
prettyPrintStep (Empty x) = "Empty cup number " ++ (show x) ++ "."
prettyPrintStep (Fill x) = "Fill cup number " ++ (show x) ++ "."
prettyPrintStep (Pour x y) = "Pour from cup number " ++ (show x) ++ " to cup number " ++ (show y) ++ "."

prettyPrintSolution :: State -> String -> State -> [Step] -> String
prettyPrintSolution init solution state [] = solution
prettyPrintSolution init solution state (step : steps) = solution ++ (prettyPrintStep step) ++ " " ++ (show $ applyStep init step state) ++
  "\n" ++ (prettyPrintSolution init solution (applyStep init step state) steps)

solveForOutput :: Int -> Maybe State -> String
solveForOutput _ Nothing = "Couldn't parse input."
solveForOutput a (Just state) | maximum state < a = "Can't be solved! (No cup is big enough.)"
                              | otherwise = case (solveProblem a state) of
  [] -> "No solution found"
  list -> prettyPrintSolution state "" (replicate (length state) 0) list

safeReadInt :: String -> Maybe Int
safeReadInt string = case (reads string :: [(Int, String)]) of
  [] -> Nothing
  (a, res) : xs -> Just a

parseInput :: String -> Maybe State
parseInput string = sequence $ map safeReadInt (words string)
