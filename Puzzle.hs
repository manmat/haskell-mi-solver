module Puzzle where

import Data.List
import Data.Array
import Data.Maybe
import Data.Graph.AStar
import qualified Data.Set as Set

type State = Array (Int, Int) Int --[[Int]]

isValidState :: State -> Bool
isValidState state = mini == 0 && minj == 0 && maxi == maxj && mini /= maxi && sort (elems state) == [0 .. (maxi + 1) * (maxj + 1) - 1] where
  ((mini, minj),(maxi, maxj)) = bounds state

defaultSolution :: Int -> State
defaultSolution n = array ((0, 0), (n - 1, n - 1)) [((i, j), n * i + j) | i <- [0 .. (n - 1)], j <- [0 .. (n - 1)]]

isSolution :: State -> Bool
isSolution state = state == (defaultSolution (maxi - mini + 1)) where
  ((mini, minj),(maxi, maxj)) = bounds state
--isSolution state = elems state == [1 .. (maxi + 1) * (maxj + 1) - 1] ++ [0] where

searchElem :: Int -> State -> Maybe (Int, Int)
searchElem a state = getZero (assocs state) where 
  getZero [] = Nothing
  getZero (x : xs) = if (snd x == a) then Just (fst x) else getZero xs

searchZero :: State -> Maybe (Int, Int)
searchZero = searchElem 0

switchPositions :: State -> (Int, Int) -> (Int, Int) -> State
switchPositions state (x1, y1) (x2, y2) = state // [((x1, y1), state ! (x2, y2)), ((x2, y2), state ! (x1, y1))]

moveZeroLeft :: State -> Maybe State
moveZeroLeft state = case searchZero state of
  Nothing -> Nothing
  Just (a, b) -> if (b == snd (fst (bounds state))) then Nothing else Just (switchPositions state (a, b) (a, b - 1))

moveZeroRight :: State -> Maybe State
moveZeroRight state = case searchZero state of
  Nothing -> Nothing
  Just (a, b) -> if (b == snd (snd (bounds state))) then Nothing else Just (switchPositions state (a, b) (a, b + 1))

moveZeroDown :: State -> Maybe State
moveZeroDown state = case searchZero state of
  Nothing -> Nothing
  Just (a, b) -> if (a == fst (snd (bounds state))) then Nothing else Just (switchPositions state (a, b) (a + 1, b))

moveZeroUp :: State -> Maybe State
moveZeroUp state = case searchZero state of
  Nothing -> Nothing
  Just (a, b) -> if (a == fst (fst (bounds state))) then Nothing else Just (switchPositions state (a, b) (a - 1, b))

moves = [moveZeroDown, moveZeroLeft, moveZeroRight, moveZeroUp]

extendState :: State -> Set.Set State
extendState state = case sequence states of
  Nothing -> Set.empty
  Just list -> Set.fromList list
  where
    states = [ f state | f <- moves , f state /= Nothing]

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

heuristicW :: State -> Int
heuristicW state = sum [distance pos (n `div` (maxi + 1), n `mod` (maxi + 1)) | (pos, n) <- list]
  where 
    list = assocs state
    ((mini, minj), (maxi, maxj)) = bounds state

solvePuzzle :: State -> Maybe [State]
solvePuzzle state = case aStar extendState (\a b -> 1) (heuristicW) isSolution state of
  Nothing -> Nothing
  Just list -> Just (state : list)

emptyBoard :: Int -> String
emptyBoard n = firstLine ++ lineForNums ++ (concat $ replicate (n - 1) (innerLines ++ lineForNums)) ++ lastLine where
  firstLine = "┌──" ++ (concat $ replicate (n - 1) "┬──") ++ "┐\n"
  lineForNums = (concat $ replicate n "│  ") ++ "│\n"
  innerLines = "├──" ++ (concat $ replicate (n - 1) "┼──") ++ "┤\n"
  lastLine = "└──" ++ (concat $ replicate (n - 1) "┴──") ++ "┘\n"

putNumberOnBoard :: Int -> String -> ((Int, Int), Int) -> String
putNumberOnBoard n board ((x, y), a) | a == 0 = take prev board ++ "  " ++ drop (prev + 2) board
                                     | a > 9 = take prev board ++ (show a) ++ drop (prev + 2) board
                                     | otherwise = take prev board ++ " " ++ (show a) ++ drop (prev + 2) board where
  prev = (3 * n + 2) * (1 + x * 2) + (3 * y + 1)

showBoard :: State -> String
showBoard state = foldl (putNumberOnBoard n) (emptyBoard n) (assocs state) where
  n = fst (snd (bounds state)) - (fst (fst (bounds state))) + 1

showSolution :: Maybe [State] -> String
showSolution Nothing = "No solution found"
showSolution (Just solution) = intercalate "\n" $ map showBoard solution

solveForOutput :: Maybe State -> String
solveForOutput Nothing = "Bad input. (Not a valid state or illegal characters)"
solveForOutput (Just state) = showSolution $ solvePuzzle state

safeReadInt :: String -> Int
safeReadInt string = case (reads string :: [(Int, String)]) of
  [] -> 0
  (a, res) : xs -> a

parseInput :: String -> Maybe State
parseInput string | isValidState state = Just state
                  | otherwise = Nothing where
  list = concatMap (\line -> map safeReadInt (words line)) $ lines string
  n = (round . sqrt . fromIntegral) $ length list
  state = if (length list == n * n) then (listArray ((0, 0), (n - 1, n - 1)) list) else (listArray ((0, 0), (0, 0)) [0])
