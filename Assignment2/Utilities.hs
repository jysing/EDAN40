module Utilities where
import Data.List

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

stringScore :: ([Char], [Char]) -> Int
stringScore ([], []) = 0
stringScore ([], (s2:ss2)) = score '-' s2 + stringScore ([], ss2)
stringScore ((s1:ss1), []) = score s1 '-' + stringScore (ss1, [])
stringScore ((s1:ss1), (s2:ss2)) = score s1 s2 + stringScore (ss1, ss2)

score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y = scoreMatch
  | otherwise = scoreMismatch

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == (maximum $ map valueFcn xs)]

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

showTup :: (Show a, Show b) => (a,b) -> String
showTup (a,b) = (show a) ++ "\n" ++ (show b) ++ "\n"
