countSheep :: [Bool] -> Int
countSheep xs = length $ filter (== True) xs