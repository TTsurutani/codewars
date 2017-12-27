import Data.List
relativelyPrime :: Integral t => t -> [t] -> [t]
relativelyPrime 0 [0] = []
relativelyPrime n l = filter' (list n) l

--篩による素数の無限リスト
sieve :: Integral a => [a] -> [a]
sieve (x:xs) = x : sieve (filter (\z -> z `mod` x /= 0) xs)
--素数リスト
primes :: Integral a => a -> [a]
primes n = takeWhile (< n) $ sieve [2..] 
--素数リストから約数のリストを作る
--nを順に素数で割っていきmod=0のみの残す
list :: Integral a => a -> [a]
list n = filter (\x -> n `mod` x == 0) $ primes n                   
--約数のリストで対象リストを順次割って余りが出るものを残す
filter' :: Integral a => [a] -> [a] -> [a]
filter' [] ls = ls
filter' (x:xs) ls = filter' xs $ filter (\y -> y `mod` x /= 0) ls

