module NQueens where

import Data.List

type Queen = (Int, Int)

isAttacking :: Queen -> Queen -> Bool
isAttacking (a, b) (c, d) = a == c || b == d || abs (a - c) == abs (b - d)

isValid :: Queen -> [Queen] -> Bool
isValid _ [] = True
isValid q qs = foldl (&&) True (map (\ x -> not $ isAttacking x q) qs)

type Board = [Queen]

validSteps :: Int -> [Board -> Board]
validSteps n = [f x | x <- [1 .. n]] where f x qs = [(length(qs) + 1, x)] ++ qs

initState = [[]] :: [Board]

isValidState :: Board -> Bool
isValidState [] = True
isValidState (q : qs) = isValid q qs && isValidState qs

applyAllSteps :: Int -> [Board] -> [Board]
applyAllSteps _ [] = []
applyAllSteps n (b : bs) = (map (\ f -> f b) (validSteps n)) ++ (applyAllSteps n bs)

solve :: Int -> [Board]
solve n = solveWithDepth n n initState where 
	solveWithDepth a 0 state = state
	solveWithDepth a b state = solveWithDepth a (b - 1) (filter (isValidState) (applyAllSteps a state))

mirrorX :: Int -> Queen -> Queen
mirrorX n (a, b) = (n+1-a, b)

mirrorXY :: Queen -> Queen
mirrorXY (a, b) = (b, a)

rotate90 :: Int -> Queen -> Queen
rotate90 n = mirrorXY . (mirrorX n)

symmetries :: Int -> [Queen] -> [[Queen]]
symmetries n qs = tr 4 (map (rotate90 n)) (tr 2 (map (mirrorX n)) [qs]) where
	tr n f = concatMap (take n . iterate f)

similar :: Int -> [Queen] -> [Queen] -> Bool
similar n a b = sort a `elem` map sort (symmetries n b)

solveUnique :: Int -> [Board]
solveUnique n = nubBy (similar n) (solve n)

emptyBoard :: Int -> String
emptyBoard n = replicate (2 * n + 1) '_' ++ "\n" ++ concat (replicate n (concat (replicate n "|_") ++ "|\n"))

addQueenToBoard :: Int -> String -> Queen -> String
addQueenToBoard n board (a, b) = take (pos - 1) board ++ "♛" ++ drop pos board where pos = a * (2 * n + 2) + b * 2

convertSolutionToString :: Int -> Board -> String
convertSolutionToString n queens = foldl (addQueenToBoard n) (emptyBoard n) queens

solveForOutput :: Int -> String
solveForOutput n = "Összesen " ++ show (length $ solve n) ++ " megoldás, ebből " ++ 
                   show (length uniques) ++ " egyedi. (A többi csak egy korábbi tükrözése vagy elforgatása)\n" ++ 
                   "Az egyedi megoldások:\n\n" ++ intercalate "\n" (map (convertSolutionToString n) uniques) where 
                     uniques = solveUnique n
