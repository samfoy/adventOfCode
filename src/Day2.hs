module Day2(paperAmount, bowAmount) where

import           Data.List  (sort)
import           Data.Maybe (fromJust)
import           Data.Text  (pack, splitOn, unpack)

paperAmount :: String -> Integer
paperAmount xs = sum $ map getPaper (lines xs)

getPaper :: String -> Integer
getPaper = (+) . minimum . fromJust . sides . components <*> fromJust . surfaceArea . components

components :: String -> [Integer]
components xs = map (read . unpack) (splitOn (pack "x") (pack xs))

surfaceArea :: [Integer] -> Maybe Integer
surfaceArea (x:y:z:_) = Just $ 2 * (x*y + x*z + y*z)
surfaceArea _ = Nothing

sides :: [Integer] -> Maybe [Integer]
sides (x:y:z:_) = Just [x*y, x*z, y*z]
sides _ = Nothing

bowAmount :: String -> Integer
bowAmount xs = sum $ map getRibbon (lines xs)

getRibbon :: String -> Integer
getRibbon = (+) . fromJust . toWrap . components <*> fromJust . toBow . components

toWrap :: [Integer] -> Maybe Integer
toWrap xs@(a:b:c:_) = Just $ sum (map (*2) (take 2 (sort xs)))
toWrap _ = Nothing

toBow :: [Integer] -> Maybe Integer
toBow (x:y:z:_) = Just $ x * y * z
toBow _ = Nothing
