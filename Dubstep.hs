module Codewars.Kata.Dubstep where

songDecoder :: String -> String
songDecoder xs = sep xs

sep :: String -> String
sep [] = []
sep [a] = [a]
sep [a,b] = [a,b]
sep (a:b:c:xs)
  | [a, b, c] == "WUB"  = ' ':sep xs
  |otherwise = a:sep(b:c:xs)

-- https://www.codewars.com/kata/dubstep/train/haskell