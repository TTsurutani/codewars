import Data.List
import Data.Char

-- validateHello :: String -> Bool
-- validateHello cs@(x:_) = any x ["hello","ciao","salut","hallo","hola","ahoj","czesc"]
songDecoder :: String -> String
songDecoder xs = lastspace $ headspace $ sep xs

sep :: String -> String
sep ('W':'U':'B':xs) = ' ' : sep xs
sep [] = []
sep (x:xs) = x : sep xs

headspace xs = if head xs == ' ' then tail xs else xs

lastspace xs = if last xs == ' ' then init xs else xs