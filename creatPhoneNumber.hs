module CreatePhoneNumber where
import Data.Char    

createPhoneNumber :: [Int] -> String
createPhoneNumber xs = "(" ++ take 3 list ++ ") " ++ take 3 (drop 3 list) ++ "-" ++ drop 6 list
                            where 
                                 list = map intToDigit xs
                                 pre = take 3 list
                                 