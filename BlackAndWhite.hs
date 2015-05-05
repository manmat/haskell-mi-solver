module BlackAndWhite where

import qualified Data.Set as Set
import Data.Graph.AStar
import Data.List
import Data.Maybe

data Tile = Black | White | Hole deriving (Eq, Show, Ord)

type State = [Tile]

lastWhite :: State -> Maybe Int
lastWhite state | list == [] = Nothing
                | otherwise = Just $ last list where
  list = elemIndices White state

firstBlack :: State -> Maybe Int
firstBlack = elemIndex Black

isSolution :: State -> Bool
isSolution state = fromMaybe False $ do
  w <- lastWhite state
  b <- firstBlack state
  return $ w < b

inv :: State -> Int
inv state = invInner 0 state where
  invInner n [] = n
  invInner n (Black : xs) = invInner ((length $ elemIndices White xs) + n) xs
  invInner n (x : xs) = invInner n xs

khi :: State -> Int
khi state | isInfixOf [Hole, Black, White] state || isInfixOf [Black, White, Hole] state = (-1)
          | otherwise = 0

heuristic :: State -> Int
heuristic state = 2 * (inv state) + (khi state)

startState :: Int -> Int -> State
startState b w = replicate b Black ++ (replicate w White) ++ [Hole]

switchBeforPrev :: State -> Maybe State
switchBeforPrev [] = Nothing
switchBeforPrev (x : []) = Nothing
switchBeforPrev (x : y : []) = Nothing
switchBeforPrev (x : y : Hole : xs) = Just $ Hole : y : x : xs
switchBeforPrev (x : xs) = fmap (x :) (switchBeforPrev xs)

switchPrev :: State -> Maybe State
switchPrev [] = Nothing
switchPrev (x : []) = Nothing
switchPrev (x : Hole : xs) = Just $ Hole : x : xs
switchPrev (x : xs) = fmap (x :) (switchPrev xs)

switchAfterNext :: State -> Maybe State
switchAfterNext [] = Nothing
switchAfterNext (x : []) = Nothing
switchAfterNext (x : y : []) = Nothing
switchAfterNext (Hole : x : y : xs) = Just $ y : x : Hole : xs
switchAfterNext (x : xs) = fmap (x :) (switchAfterNext xs)

switchNext :: State -> Maybe State
switchNext [] = Nothing
switchNext (x : []) = Nothing
switchNext (Hole : x : xs) = Just $ x : Hole : xs
switchNext (x : xs) = fmap (x :) (switchNext xs)

moves = [switchAfterNext, switchNext, switchPrev, switchBeforPrev]

extendState :: State -> Set.Set State
extendState state = Set.fromList $ catMaybes [move state | move <- moves]

solvePuzzle :: Int -> Int -> Maybe [State]
solvePuzzle b w = aStar extendState (\a b -> 1) heuristic isSolution (startState b w)

stateToString :: State -> String
stateToString state = concatMap tileToString state ++ "\n" where
  tileToString Black = "■ "
  tileToString White = "□ "
  tileToString Hole = "_ "

solveForOutput :: Int -> Int -> String
solveForOutput a b = case solvePuzzle a b of
  Nothing -> "No solution found"
  Just list -> concatMap stateToString (startState a b : list)
