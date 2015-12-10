module Day3(housesVisited, housesWithRobo) where

import       Data.List(nub,foldl')
import       Control.Monad

data Position = Position {
  north::Int,
  east::Int
} deriving(Show,Read,Eq,Ord)

initial = Position 0 0

housesVisited :: String -> Int
housesVisited = length . nub . scanl visit initial

visit :: Position -> Char -> Position
visit (Position n e) x = case x of
  '<' -> Position n (e-1)
  '>' -> Position n (e+1)
  '^' -> Position (n+1) e
  'v' -> Position (n-1) e

housesWithRobo :: String -> Int
housesWithRobo = length . nub . ((++) . scanl visit initial . santaHouses <*> scanl visit initial . roboHouses)

santaHouses :: String -> String
santaHouses = each 2

roboHouses :: String -> String
roboHouses = each 2 . tail

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)
