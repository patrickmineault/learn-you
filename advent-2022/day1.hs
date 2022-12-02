import Data.List

-- Equivalent to splitOn
-- https://hackage.haskell.org/package/split-0.2.3.5/docs/Data-List-Split.html
chop :: [String] -> [[String]]
chop a = foldr (chopr) ([[]]) a where
    chopr "" (groups) = [] : groups
    chopr b (h:groups) = (b:h) : groups

main :: IO()
main = do
    f <- readFile "data/day1.txt"
    let theLines = lines f
    let groups = chop theLines
    let sumPer = map (\a -> sum [read x|x<-a]) groups
    let tops = reverse $ sort sumPer
    print $ show (sum (take 1 tops))
    print $ show (sum (take 3 tops))