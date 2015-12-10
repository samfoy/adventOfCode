module Day1
    ( directions,
      positionAt
    ) where

directions :: String -> Integer
directions = sum . map convertParen

convertParen :: Char -> Integer
convertParen ')' = -1
convertParen '(' = 1
convertParen _ = 0

positionAt :: Integer -> String -> Integer
positionAt i xs = toInteger (length (takeWhile (/= i) (scanl (+) 0 (map convertParen xs))))
