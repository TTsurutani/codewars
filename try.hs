import Data.List
import Data.Char
-- |
-- >>> nb_year 1500 5 100 5000
-- 15
-- 

-- p0, percent, 
-- aug (inhabitants coming or leaving each year), p (population to surpass)
--nbYear :: Int -> Double -> Int -> Int -> Int
-- nbYear :: (Fractional t, Ord t) => t -> t -> t -> t -> Int

nbYear p0 percent aug p = length $ unfoldr f p0
         where
            f x
             | x >= p    = Nothing
             | otherwise = Just (x , x * (1.0 + percent * 0.01) + aug)

howMuchILoveYou :: Int -> String
howMuchILoveYou nbPetals = ["I love you","a little","a lot","passionately","madly","not at all"] !! (nbPetals `mod` 7 - 1)

areYouPlayingBanjo :: String -> String
areYouPlayingBanjo name@('R':_) = name ++ " plays banjo"
areYouPlayingBanjo name@('r':_) = name ++ " plays banjo"
areYouPlayingBanjo ys = ys ++ " does not play banjo"

-- capitalize "abcdef" = ["AbCdEf", "aBcDeF"]
capitalize :: String -> [String]
capitalize str = [capitalize' odd str, capitalize' even str]

capitalize' func = zipWith f [1..] 
  where 
    f n c
     | func n = toUpper c
     | otherwise = c

-- evaporator :: Double -> Double -> Double -> Integer
evaporator _ evap_per_day threshold = length $ unfoldr f 100
  where f x
          | x < threshold = Nothing
          | otherwise = Just (x , x - x*evap_per_day*0.01)

{--
          split :: Char -> String -> [String]
split x ys = split' x [] ys

split' :: Char -> [Char] -> [Char]
split' x z (y:ys)
  | x == y    = [z,ys]
  | otherwise = [z ++ y, split' x ys]
  --}

test :: Double -> Double
test x = floor (x * realToFrac 100.0)
{-- 
twoDecimalPlaces :: Double -> Double
twoDecimalPlaces x
    | j < '5'   = floor x' / 100 :: Doubel
    | otherwise = ceiling x' / 100 ::Double
       where
         x' = realToFrac x * 100
         j  = last $ show x'  
--}         