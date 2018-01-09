import Data.List
import Data.Char
relativelyPrime :: Integral t => t -> [t] -> [t]
relativelyPrime 0 [0] = []
relativelyPrime n l = filter' (list n) l

-- �k�� pn!P��
sieve :: Integral a => [a] -> [a]
sieve (x:xs) = x : sieve (filter (\z -> z `mod` x /= 0) xs)
-- p��
primes :: Integral a => a -> [a]
primes n = takeWhile (< n) $ sieve [2..]
-- p��K�pn�\�
--n�k pgrcfDMmod=0nn�Y
list :: Integral a => a -> [a]
list n = filter (\x -> n `mod` x == 0) $ primes n
--pn��g�a�!rcfY�L���n��Y
filter' :: Integral a => [a] -> [a] -> [a]
filter' [] ls = ls
filter' (x:xs) ls = filter' xs $ filter (\y -> y `mod` x /= 0) ls

makeValley :: [Int] -> [Int]
makeValley arr = reverse rlist ++ [bottom] ++ llist
    where
      llist = if head lista > head listb then listb else lista
      rlist = if head listb > head lista then lista else listb
      lista = [v| (i,v)<-tail list, even i]
      listb = [v| (i,v)<-tail list, odd i]
      (_,bottom) = head list
      list = zip [1..] $ sort arr

checkDigit :: Int -> Int -> Int -> Int -> Bool 
checkDigit number index1 index2 digit  = intToDigit digit `elem` take (to - from + 1)  drop from list
        where
         list = show number
         from = if index1 > index2 then index2 else index1
         to = if from == index1 then index2 else index1


sevenAte9 :: String -> String
sevenAte9 [] = []
sevenAt9 ('7':'9':'7':xs) = '7':sevenAt9 xs
sevenAt9 (x:xs) = x : sevenAt9 xs

wordCount :: Char -> String -> Int
wordCount _ [] = 0
wordCount x (y:ys)
  | x == y = 1 + wordCount x ys
  | otherwise = wordCount x ys

countList :: String -> [(Char,Int)] 
countList str = map (\ x -> (x, wordCount x str)) $ nub str