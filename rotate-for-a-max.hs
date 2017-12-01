module Codewars.G964.Maxrot where
import Data.List

maxRot :: Integer -> Integer
maxRot n = read $ maximum $ f $ show n ::Integer

f :: String -> [String]
f string = unfoldr g (1,string)
  where
    g (n,str)
     | n == length str + 1 = Nothing
     | otherwise = Just (str,(n + 1 ,uncurry rotate (n, str)))

rotate :: Int -> String -> String
rotate 0 xs = xs
rotate n xs = as ++ bs ++ [b]
   where (as, b : bs) = splitAt (n - 1) xs
          

-- https://www.codewars.com/kata/rotate-for-a-max/train/haskell
