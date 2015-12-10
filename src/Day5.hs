module Day5(numberNice, numberNice2, isNice2, pairOfTwos, hasSandwich, clean3s) where

import           Data.List (group, intersect, isInfixOf, nub)


bads = ["ab", "cd", "pq", "xy"]

numberNice :: String -> Int
numberNice = length . filter isNice . lines

isNice :: String -> Bool
isNice xs = v3 xs && dub xs && noBad xs

v3 :: String -> Bool
v3 = (>= 3) . length . filter (`elem` "aeiou")

dub :: String -> Bool
dub = any ((1 <) . length) . group

noBad :: String -> Bool
noBad xs = not (any (`isInfixOf` xs) bads)

numberNice2 :: String -> Int
numberNice2 = length . filter isNice2 . lines

isNice2 :: String -> Bool
isNice2 xs = pairOfTwos xs && hasSandwich xs

pairOfTwos :: String -> Bool
pairOfTwos xs = length (nub (takePairs (clean3s xs))) /= length (takePairs (clean3s xs))

takePairs :: String -> [String]
takePairs [x] = []
takePairs (x:y:xs) = (x : y : "") : takePairs (y:xs)

clean3s :: String -> String
clean3s (a:b:c:d:xs) = if length (nub (take 4 (a:b:c:d:xs))) == 1 then a : b : c : d : clean3s xs else a : clean3s (b:c:d:xs)
clean3s (x:y:z:xs) = if length (nub (take 3 (x:y:z:xs))) == 1 then clean3s (y:z:xs) else x : clean3s (y:z:xs)
clean3s xs  = xs

hasSandwich :: String -> Bool
hasSandwich = not . null . sandwich

sandwich :: String -> [String]
sandwich (x:y:z:xs) = if x == z then [x,y,z] : sandwich (y:z:xs) else sandwich (y:z:xs)
sandwich xs = []

--Ended up just doing a grep for part 2.
-- cat input/day5 | egrep '(..).*\1' | egrep '(.).\1' | wc
