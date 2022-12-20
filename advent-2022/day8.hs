import Data.List

laserCut :: [Int] -> [Bool]
laserCut a = map snd $ tail $ scanl acc (-1, False) a
    where
        acc = \(theMax, vis) newVal -> (max newVal theMax, newVal > theMax)

scoreAt :: [[Int]] -> (Int, Int) -> Int
scoreAt a (x, y) = topScore * bottomScore * leftScore * rightScore
    where
        topScore = scoreDir a (x, y) (0, -1)
        bottomScore = scoreDir a (x, y) (0, 1)
        leftScore = scoreDir a (x, y) (-1, 0)
        rightScore = scoreDir a (x, y) (1, 0)

scoreDir :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int
scoreDir a (x, y) (dx, dy) = theScore
    where
        theScore = walk a (x, y) (dx, dy) ((a !! y) !! x) 0

walk :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int
walk a (x, y) (dx, dy) maxScore steps
    | x + dx < 0 = steps
    | x + dx >= length (a !! 0) = steps
    | y + dy < 0 = steps
    | y + dy >= length a = steps
    | num < maxScore = (walk a (x+dx, y+dy) (dx, dy) maxScore (steps + 1))
    | otherwise = steps + 1
    where
        num = (a !! (y + dy)) !! (x + dx)

main :: IO ()
main = do
    l <- readFile "data/day8.txt"
    let squares = map (map (\x -> read [x])) $ lines l
        vis0 = map laserCut squares
        vis1 = map (reverse . laserCut . reverse) squares
        vis2 = transpose $ map laserCut (transpose squares)
        vis3 = transpose $ map (reverse . laserCut . reverse) (transpose squares)

        combo0 = zipWith (zipWith (||)) vis0 vis1
        combo2 = zipWith (zipWith (||)) vis2 vis3
        combo1 = zipWith (zipWith (||)) combo0 combo2

        ranges = [(x, y) | x <- [0..((length $ squares !! 0) - 1)], y <- [0..((length squares) - 1)]]
        nums = map (scoreAt squares) ranges

    print $ sum $ map fromEnum $ concat combo1
    print $ maximum nums
