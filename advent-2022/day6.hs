allDistinct :: String -> Bool
allDistinct a = fst $ (foldl cumDistinct (True, "") a)
    where
        cumDistinct = \(distinct, lst) el -> (distinct && not (el `elem` lst), el:lst)

findAllDistinct :: String -> Int -> Int -> Int
findAllDistinct (a:as) delta n
    | allDistinct (take n (a:as)) = delta + n
    | otherwise = findAllDistinct as (delta + 1) n

main :: IO ()
main = do
    f <- readFile "data/day6.txt"
    print $ show (findAllDistinct f 0 4)
    print $ show (findAllDistinct f 0 14)