chop :: String -> (Char, Char)
chop a = (a !! 0, a !! 2)

-- rock paper scissors
-- 0, 3, 6 -> lose, draw, win
-- 1, 2, 3
scoreEach :: (Char, Char) -> Int
scoreEach ('A', 'X') = 3 + 1
scoreEach ('B', 'X') = 0 + 1
scoreEach ('C', 'X') = 6 + 1
scoreEach ('A', 'Y') = 6 + 2
scoreEach ('B', 'Y') = 3 + 2
scoreEach ('C', 'Y') = 0 + 2
scoreEach ('A', 'Z') = 0 + 3
scoreEach ('B', 'Z') = 6 + 3
scoreEach ('C', 'Z') = 3 + 3

-- X: lose, Y: draw, Z: win
scoreEach2 :: (Char, Char) -> Int
scoreEach2 ('A', 'X') = 0 + 3
scoreEach2 ('B', 'X') = 0 + 1
scoreEach2 ('C', 'X') = 0 + 2
scoreEach2 ('A', 'Y') = 3 + 1
scoreEach2 ('B', 'Y') = 3 + 2
scoreEach2 ('C', 'Y') = 3 + 3
scoreEach2 ('A', 'Z') = 6 + 2
scoreEach2 ('B', 'Z') = 6 + 3
scoreEach2 ('C', 'Z') = 6 + 1

main :: IO()
main = do
    f <- readFile "data/day2.txt"
    let theLines = lines f
    let parsed = map chop theLines
    print ( show ( sum $ map scoreEach parsed))
    print ( show ( sum $ map scoreEach2 parsed))