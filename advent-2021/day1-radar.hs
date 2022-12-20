nincreasing :: (Num a, Ord a) => [a] -> Int
nincreasing xs = fst $ foldl ninc (0, head xs) xs

ninc :: (Num a, Ord a) => (Int, a) -> a -> (Int, a)
ninc (acc, lastEl) currVal = if currVal > lastEl then
                                (acc + 1, currVal)
                              else
                                (acc, currVal)

slidingWin3 :: (Num a) => [a] -> [a]
slidingWin3 (a:b:xs) = zipWith3 (\x y z -> x+y+z) xs (b:xs) (a:b:xs)

main :: IO()
main = do
    f <- readFile "day1-input.txt"
    let l = lines f
    let m = [read x :: Int | x <- l]
    print . show $ nincreasing m
    let threec = slidingWin3 m
    print . show $ nincreasing threec
