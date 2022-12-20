import Data.List.Split

data Assignment = Assignment Int Int

data AssignmentPair = AssignmentPair Assignment Assignment

instance Show AssignmentPair where
    -- Implement the 'show' function for the 'Point' data type
    show (AssignmentPair (Assignment l1 r1) (Assignment l2 r2)) = "Pair(" ++ show l1 ++ "-" ++ show r1 ++ "," ++ show l2 ++ "-" ++ show r2 ++ ")"

parseAssignmentPair :: String -> AssignmentPair
parseAssignmentPair a = let [firstHalf, secondHalf] = splitOn "," a
                            [num11, num12] = splitOn "-" firstHalf
                            [num21, num22] = splitOn "-" secondHalf
                        in AssignmentPair (Assignment (read num11 :: Int) (read num12 :: Int)) (Assignment (read num21 :: Int) (read num22 :: Int))

scoreOverlapping :: AssignmentPair -> Int
scoreOverlapping (AssignmentPair (Assignment l1 r1) (Assignment l2 r2))
    | (l1 >= l2) && (r1 <= r2) = 1
    | (l2 >= l1) && (r2 <= r1) = 1
    | otherwise = 0

scoreAnyOverlapping :: AssignmentPair -> Int
scoreAnyOverlapping (AssignmentPair (Assignment l1 r1) (Assignment l2 r2))
    | (l1 > r2) = 0
    | (l2 > r1) = 0
    | otherwise = 1

isOverlapping :: AssignmentPair -> Bool
isOverlapping (AssignmentPair (Assignment l1 r1) (Assignment l2 r2))
    | (l2 > r1) = False
    | (l1 > r2) = False
    | otherwise = True

main :: IO()
main = do
    f <- readFile "data/day4.txt"
    let assignments = map parseAssignmentPair (lines f)
        noverlapping = sum $ map scoreOverlapping assignments
        noverlapping2 = sum $ map scoreAnyOverlapping assignments
    print $ show noverlapping
    print $ show noverlapping2
    --print $ show (isOverlapping (AssignmentPair (Assignment 10 10) (Assignment 10 10)))
    --print $ show (filter isOverlapping assignments)