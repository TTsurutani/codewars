module Codewars.G964.Accumule where
import Data.Char    
accum :: String -> String
accum = concat.zipWith make [0, 1 ..]

make :: Int -> Char -> String
make 0 c = [toUpper c]
make n c = '-' : toUpper c : replicate n (toLower c)

{-- 
zipしてインデックス付
インデックス情報を元に文字列生成
　０のとき大文字化のみ
　1以上で先頭に-、後ろに小文字化した文字をreplicate
--}