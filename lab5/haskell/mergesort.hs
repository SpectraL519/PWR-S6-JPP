merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergesort  :: Ord a => [a] -> [a]
mergesort  []  = []
mergesort  [x] = [x]
mergesort  xs  = merge (mergesort left) (mergesort right)
    where
        (left, right) = halve xs


main :: IO ()
main = do
    let unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
    let sortedList = mergesort unsortedList

    print unsortedList
    print sortedList
