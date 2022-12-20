import qualified Data.Set as Set
import Data.List.Split

type Direction = Char
type HeadPosition = (Int, Int)
type TailPosition = (Int, Int)
type Visited = Set.Set (Int, Int)
type Positions = [HeadPosition]

moveOne :: Visited -> HeadPosition -> TailPosition -> Direction -> (Visited, HeadPosition, TailPosition)
moveOne v h t 'U' = (v2, h2, t2)
    where
        h2 = (fst h, (snd h) - 1)
        t2 = if abs ((snd h2) - (snd t)) == 2 then
            (fst h, (snd h2) + 1) else t
        v2 = Set.insert t2 v
moveOne v h t 'D' = (v2, h2, t2)
    where
        h2 = (fst h, (snd h) + 1)
        t2 = if abs ((snd h2) - (snd t)) == 2 then
            (fst h, (snd h2) - 1) else t
        v2 = Set.insert t2 v
moveOne v h t 'L' = (v2, h2, t2)
    where
        h2 = ((fst h) - 1, snd h)
        t2 = if abs ((fst h2) - (fst t)) == 2 then
            ((fst h2) + 1, snd h2) else t
        v2 = Set.insert t2 v
moveOne v h t 'R' = (v2, h2, t2)
    where
        h2 = ((fst h) + 1, snd h)
        t2 = if abs ((fst h2) - (fst t)) == 2 then
            ((fst h2) - 1, snd h2) else t
        v2 = Set.insert t2 v

snap :: Int -> Int
snap (-2) = -1
snap 2 = 1

resolve :: Positions -> Int -> Positions
resolve ps which
    | which >= (length ps) - 1 = ps
    | otherwise = resolved
    where
        (x0, y0) = ps !! which
        (x1, y1) = ps !! (which + 1)
        dx = abs (x0 - x1)
        dy = abs (y0 - y1)

        t = if (dx >= 2) && (dy >= 2)
            then (x0 + snap(x1-x0), y0 + snap(y1-y0))
            else if dx >= 2 
                    then (x0 + snap(x1 - x0), y0) 
                    else if dy >= 2
                        then (x0, y0 + snap(y1 - y0)) else (x1, y1)
        p2 = (take (which + 1) ps) ++ [t] ++ (drop (which + 2) ps)
        resolved = resolve p2 (which + 1)

moveOne2 :: Visited -> Positions -> Direction -> (Visited, Positions)
moveOne2 v (h:ps) 'U' = (v2, p2)
    where
        h2 = (fst h, (snd h) - 1)
        p2 = resolve (h2:ps) 0
        v2 = Set.insert (p2 !! ((length p2) - 1)) v
moveOne2 v (h:ps) 'D' = (v2, p2)
    where
        h2 = (fst h, (snd h) + 1)
        p2 = resolve (h2:ps) 0
        v2 = Set.insert (p2 !! ((length p2) - 1)) v
moveOne2 v (h:ps) 'L' = (v2, p2)
    where
        h2 = ((fst h) - 1, snd h)
        p2 = resolve (h2:ps) 0
        v2 = Set.insert (p2 !! ((length p2) - 1)) v
moveOne2 v (h:ps) 'R' = (v2, p2)
    where
        h2 = ((fst h) + 1, snd h)
        p2 = resolve (h2:ps) 0
        v2 = Set.insert (p2 !! ((length p2) - 1)) v

moveMulti :: Visited -> HeadPosition -> TailPosition -> Direction -> Int -> (Visited, HeadPosition, TailPosition)
moveMulti v h t d 0 = (v, h, t)
moveMulti v h t d n = moveMulti v2 h2 t2 d (n - 1)
    where
        (v2, h2, t2) = moveOne v h t d

moveMulti2 :: Visited -> Positions -> Direction -> Int -> (Visited, Positions)
moveMulti2 v p d 0 = (v, p)
moveMulti2 v p d n = moveMulti2 v2 p2 d (n - 1)
    where
        (v2, p2) = moveOne2 v p d


runInstruction :: (Visited, HeadPosition, TailPosition) -> String -> (Visited, HeadPosition, TailPosition)
runInstruction (v, h, t) line = moveMulti v h t (direction !! 0) n2
    where
        [direction, n] = splitOn " " line
        n2 = read n :: Int

runInstruction2 :: (Visited, Positions) -> String -> (Visited, Positions)
runInstruction2 (v, p) line = moveMulti2 v p d n2
    where
        [direction, n] = splitOn " " line
        d = direction !! 0
        n2 = read n :: Int

runInstructions :: [String] -> (Visited, HeadPosition, TailPosition)
runInstructions instructions = foldl (runInstruction) (Set.empty :: Visited, (0, 0) :: HeadPosition, (0, 0) :: TailPosition) instructions

runInstructions2 :: [String] -> (Visited, Positions)
runInstructions2 instructions = foldl (runInstruction2) (Set.empty, zeros) instructions
    where
        zeros = map (\x -> (0, 0)) [1..10]


main :: IO()
main = do
    f <- readFile "data/day9.txt"
    let instructions = lines f
    --    (uniquePositions, _, _) = runInstructions instructions
    --print $ show uniquePositions
    --print $ show $ Set.size uniquePositions

    -- Part 2
    let (uniquePositions, positions) = runInstructions2 instructions
    --print $ show $ length uniquePositions
    --let sequence = runInstructions2 instructions
    print $ show $ Set.size uniquePositions