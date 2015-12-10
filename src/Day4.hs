module Day4(firstWithX,firstWith6) where

import           Data.Hash.MD5

allInts = [1..]


firstWithX :: String -> Int -> String
firstWithX key x = drop (length key) (head $ filter (isX x) $ map (\x -> key ++ show x) allInts)

firstWith6 key = drop (length key) (head $ filter (isX 6) $ map (\x -> key ++ show x) allInts)

isX :: Int -> String -> Bool
isX 5 x = "00000" == take 5 (md5s (Str x))
isX 6 x = "000000" == take 6 (md5s (Str x))
isX a x = concat (replicate a "0") == take a md where
  md = md5s (Str x)
