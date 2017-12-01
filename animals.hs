module Animals where
animals :: Int -> Int -> Maybe (Int,Int)
animals 0 0 = Just (0,0)
animals heads legs
  | heads < 0 || legs < 0     = Nothing
  | cows  < 0 || chickens < 0 = Nothing
  | isValid heads legs        = Just (chickens,cows)
  | otherwise = Nothing
 where cows = legs `div ` 2 - heads
       chickens = heads - cows 

-- https://www.codewars.com/kata/heads-and-legs/train/haskell       