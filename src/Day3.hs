module Day3(housesVisited, housesWithRobo) where

import       Data.List(nub,foldl')
import       Control.Monad

data Position = Position {
  north::Integer,
  east::Integer
} deriving(Show,Read,Eq,Ord)

initial = Position 0 0

housesVisited :: String -> Integer
housesVisited = toInteger . length . nub . scanl visit initial

visit :: Position -> Char -> Position
visit (Position n e) x = case x of
  '<' -> Position n (e-1)
  '>' -> Position n (e+1)
  '^' -> Position (n+1) e
  'v' -> Position (n-1) e

housesWithRobo :: String -> Integer
housesWithRobo = toInteger . length . nub . ((++) . scanl visit initial . santaHouses <*> scanl visit initial . roboHouses)

santaHouses :: String -> String
santaHouses = each 2

roboHouses :: String -> String
roboHouses = each 2 . tail

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)
