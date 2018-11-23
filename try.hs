import Data.List
import Data.Char
import Data.Maybe (fromJust)


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


makeValley :: [Int] -> [Int]
makeValley arr = reverse rlist ++ [bottom] ++ llist
    where
      llist = if head lista > head listb then listb else lista
      rlist = if head listb > head lista then lista else listb
      lista = [v| (i,v)<-tail list, even i]
      listb = [v| (i,v)<-tail list, odd i]
      (_,bottom) = head list
      list = zip [1..] $ sort arr
{--
checkDigit :: Int -> Int -> Int -> Int -> Bool 
checkDigit number index1 index2 digit  = intToDigit digit `elem` take (to - from + 1)  drop from list
        where
         list = show number
         from = if index1 > index2 then index2 else index1
         to = if from == index1 then index2 else index1
--}

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

isValidWalk :: String -> Bool
isValidWalk walk
  | length (take 10 walk) /= 10 = False
  | otherwise = (f "ns", f "ew") == (0,0)
    where
     f xs = sum $ map toDigit $ filter (`elem` xs) walk

toDigit :: Char -> Int
toDigit 'n' = 1
toDigit 's' = -1
toDigit 'e' = 1
toDigit 'w' = -1

averages :: Maybe [Double] -> [Double]
averages (Just xs) = zipWith (\ x y-> (x+y)/2) xs (tail xs)
{--
pattern :: Int -> String
pattern 0 = []
pattern n = pattern' n n

pattern' 0 _ = []
pattern' n 1 = f n 1
pattern' n m = f n m ++ "\n" ++ pattern' n (m-1)

f n m = take m $ concatMap show [n,n-1..1] 
--}
pattern' :: Int -> [[Int]]
pattern' n = unfoldr f [n,n-1..1]
    where
      f x
       | null x = Nothing
       | otherwise = Just (x,init x)


toLeetSpeak :: String -> String
toLeetSpeak = map conv

conv :: Char -> Char
conv x = fromJust $ lookup x llist

llist = [
  ('A','@'),
  ('B','8'),
  ('C','('),
  ('D','D'),
  ('E','3'),
  ('F','F'),
  ('G','6'),
  ('H','#'),
  ('I','!'),
  ('J','J'),
  ('K','K'),
  ('L','1'),
  ('M','M'),
  ('N','N'),
  ('O','0'),
  ('P','P'),
  ('Q','Q'),
  ('R','R'),
  ('S','$'),
  ('T','7'),
  ('U','U'),
  ('V','V'),
  ('W','W'),
  ('X','X'),
  ('Y','Y'),
  ('Z','2')]

solve :: String -> String

solve str = unwords $ unfoldr f (revlist,lenList)
             where
              revlist = reverse.concat $ words str
              lenList = map length $ words str
              
f (rs, l : ls)
  | null ls = Nothing
  | otherwise = Just (take l rs , (drop l rs,ls)) 


--sumFromString :: String -> Integer
--sumFromString = sum.read.words.map con

con :: Char -> Char
con n
  | isDigit n = n
  | otherwise = ' '

sumTimesTables :: [Integer] -> Integer -> Integer -> Integer
sumTimesTables tbl n m = sum $ map ((sum . zipWith (*) [n .. m]) . repeat) tbl
    
d :: [(Int,Int)] -> Int
d list = sum $ map pair list

pair :: (Int,Int) -> Int
pair (a,b) = b - a + 1

sam :: Int -> Int -> [Int]
sam s m
  | y /= 0 = []
  | otherwise = [x,s-x]
  where
    (x,y) = properFraction $ solveX s m

solveX :: Int -> Int -> Float
solveX s' m' = s/2 + sqrt (s^2 - 4*m)/2
  where
    s = fromIntegral s'
    m = fromIntegral m'

scoreTest :: (Integral a) => [a] -> a -> a -> a -> a
scoreTest li p0 p1 p2 = score 0 p0 li + score 1 p1 li - score 2 p2 li

score :: (Integral a) => a -> a -> [a] -> a
score x p = sum.map (const p).filter (== x) 

wcount :: Char -> String -> Int
wcount x str = length $ filter (== x) str

ff str n = d*ct + ct'
  where
    (d,m) = n `divMod` length str
    a = head str
    ct = wcount a str
    ct' = wcount a $ take m str

--ceebbcb
--817723
--0

{--
seaSick :: String -> String
seaSick sea
  | ratio >= 0.2 = "Throw Up"
  | otherwise    = "No Problem"
  where
    list = zip [0..] sea
    list' = zip list (tail list)
    match = fromIntegral
            .length
            .nub.map head
            
            .filter judgeT
            $ list'
    ratio = match / (fromIntegral.length $ list)
--}
judgeT ::((Int,Char),(Int,Char)) -> Bool
judgeT (( _ ,'~'),( _ ,'_')) = True
judgeT (( _ ,'_'),( _ ,'~')) = True
judgeT _ = False

climb  = sort . unfoldr f
 where 
  f n 
   | n == 0 = Nothing
   | otherwise = Just (n,n `div` 2)

rep :: String -> String
rep [] = []
rep (x:xs)
  | x `elem` "aiueo" = x : rep xs
  | otherwise = ' ' : rep xs


nikouwa :: [Integer] -> Integer
nikouwa = sum . map (^2)  

correct :: String -> String
correct = map convv

convv :: Char -> Char
convv 'S' = '5'
convv 'O' = '0'
convv 'I' = '1'
convv c = c

countSheep n = concatMap ((++ " sheep...") . show) [1 .. n]