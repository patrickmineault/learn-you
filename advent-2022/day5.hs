import Data.List
import Data.List.Split

type Stack = [Char]

data Move = Move Int Int Int deriving Show

parseMove :: String -> Move
parseMove as = 
    Move n from to 
    where
        subs = splitOn " " as
        n = read $ subs !! 1
        from = (read $ subs !! 3) - 1
        to = (read $ subs !! 5) - 1

parseStack :: String -> Stack
parseStack line = map (line !!) ([i + 1 | i <- [0..((length line) - 3)], i `mod` 4 == 0])

readInstructions :: [String] -> ([Stack], [Move]) -> ([Stack], [Move])
readInstructions (a:as) (stacks, moves) = readInstructions (as) (newStacks, newMoves) where
    newStacks = if isInfixOf "[" a
                    then ((parseStack a):stacks)
                    else stacks
    newMoves = if isInfixOf "move" a
                    then ((parseMove a):moves)
                    else moves
readInstructions [] (stack, moves) = (stack, moves)

chopStack :: [Stack] -> Int -> Int -> ([Stack], Stack)
chopStack stacks col num = (newStacks, dropped) where
    middle = drop num (stacks !! col)
    dropped = take num (stacks !! col)
    newStacks = (take col stacks) ++ [middle] ++ (drop (col+1) stacks)


stackStack :: [Stack] -> Int -> Stack -> [Stack]
stackStack stacks col newMaterial = newStacks where
    middle = newMaterial ++ (stacks !! col)
    newStacks = (take col stacks) ++ [middle] ++ (drop (col+1) stacks)


runInstructions :: ([Stack], [Move]) -> ([Stack], [Move])
runInstructions (stacks, m:ms) = 
    runInstructions (movedStacks, ms) 
    where
        Move n from to = m
        (stacks2, newMaterial) = chopStack stacks from n
        movedStacks = stackStack stacks2 to ( newMaterial)
runInstructions (stacks, []) = (stacks, [])


main :: IO()
main = do
    f <- readFile "data/day5.txt"
    let (seqs, instructions) = readInstructions (lines f) ([], [])
        stacks = transpose seqs
        trueStacks = map (reverse . takeWhile (' ' /=)) stacks
        trueInstructions = reverse instructions
        (movedStacks, _) = runInstructions (trueStacks, trueInstructions)
        topThings = map (\x -> x !! 0) movedStacks

    print $ show topThings
    
