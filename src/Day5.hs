module Day5(numberNice, isNice, v3, dub, noBad) where

import           Data.List (group, intersect, isInfixOf)


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
