import Data.Char (digitToInt, toLower)

sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)
primes = sieve [2..]

{--
barasu :: Int -> (Int,[Int])
barasu n
 | n `mod` x == 0 = (n,x:barasu n)
 | otherwese = (n,barasu xs)
 where
    x:xs = primes
--}
select :: Int -> [a] -> a
select 1 xs     = head xs
select n (x:xs) = select (n - 1 ) xs

elementAt :: Int -> [a] -> Maybe a
elementAt n ls@(x:xs)
  | n < 1 = Nothing
  | n > length ls = Nothing
  | n == 1 = Just x
  | otherwise = elementAt (n - 1) xs

squareDigit num = (read::String->Int) $ concatMap (show . (^ 2) . digitToInt) (show num)
-- squareDigit num = (read::String->Int) $ concat $ map show $ map (^2) $  map digitToInt  $ show num

highAndLow :: String -> String
highAndLow input = show (maximum list) ++ " " ++ show (minimum list)
  where list = words input
 

xo :: String -> Bool
xo str = a == b
 where 
     str' = map toLower str
     a = length $ filter (== 'x') str
     b = length $ filter (== 'o') str  