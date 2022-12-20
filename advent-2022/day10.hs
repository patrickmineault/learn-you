import Data.List
import Data.List.Split

type Register = Int
type Clock = Int
type State = (Register, Clock)
type Instruction = String

execOne :: State -> Instruction -> State
execOne (register, clock) instruction
    | isPrefixOf "noop" instruction = (register, clock + 1)
    | isPrefixOf "addx" instruction = (register + num, clock + 2)
    where
        [_, nums] = splitOn " " instruction
        num = read nums

execute :: [Instruction] -> [State]
execute a = scanl execOne ((1, 1) :: State) a

infill :: [State] -> [State]
infill (a:b:as)
    | snd b == (snd a) + 1 = a:(infill (b:as))
    | snd b == (snd a) + 2 = a:(fst a, 1 + (snd a)):(infill (b:as))
infill a = a

spriteThere :: State -> Char
spriteThere (register, clock)
    | (p + 2 == c) || (p + 1 == c) || (p == c) = '#'
    | otherwise = '.'
    where
        c = (clock - 1) `mod` 40
        p = (register - 1)

main :: IO()
main = do
    f <- readFile "data/day10.txt"
    let instructions = lines f
        execution = execute instructions
        fullState = init $ infill execution
        subStates = [fullState !! 19, fullState !! 59, fullState !! 99, 
                     fullState !! 139, fullState !! 179, fullState !! 219]
        vals = map (\x -> (*) (fst x) (snd x)) subStates
        display = chunksOf 40 $ map spriteThere fullState

    print $ sum vals
    putStr $ intercalate "\n" display