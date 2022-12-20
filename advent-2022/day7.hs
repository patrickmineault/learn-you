import Data.Maybe
import Data.List
import Data.Char
import Data.List.Split

data File = File String Int deriving Show

-- name parentName files size
data Dir = Dir String String [File] Int deriving Show

-- same directory == same name
instance Eq Dir where
    (==) :: Dir -> Dir -> Bool
    (==) l r = same
        where
            Dir a1 _ _ _ = l
            Dir a2 _ _ _ = r
            same = a1 == a2

parseFile :: [String] -> [Dir] -> [Dir]
parseFile (line:lines) (d:ds)
    | isInfixOf "cd .." line = parseFile lines (ds ++ [d])
    | isInfixOf "cd " line = parseFile lines ((Dir (cname ++ name ++ "/") cname [] 0):d:ds)
    | isDigit (line !! 0) = parseFile lines (d2:ds)
    | otherwise = parseFile lines (d:ds) -- eat the input
    where
        name = (splitOn " " line) !! 2
        Dir cname parent files _ = d
        [filesize, filename] = splitOn " " line
        d2 = Dir cname parent ((File filename (read filesize)):files) 0
parseFile [] d = d

bubbleUp :: [Dir] -> Maybe Int -> Int -> [Dir]
bubbleUp as Nothing _ = as
bubbleUp as (Just idx) size = bubbleUp as2 parentIdx size
    where
        Dir name parent files currentSize = (as !! idx)
        newdir = Dir name parent files (currentSize + size)
        -- splice things in
        as2 = (take idx as) ++ [newdir] ++ (drop (idx+1) as)
        parentIdx = if parent == ""
            then 
                Nothing
            else
                elemIndex (Dir parent "" [] 0) as


propagateSize :: [Dir] -> Int -> [Dir]
propagateSize ds num
    | (num == length ds) = ds
    | otherwise          = propagateSize bubbledUp (num + 1)
    where
        Dir name parent files currentSize = (ds !! num)
        totalSize = sum $ map (\(File name size) -> size) files
        bubbledUp = bubbleUp ds (Just num) totalSize


main :: IO ()
main = do
    f <- readFile "data/day7.txt"
    let bareTree = parseFile (tail $ lines f) [Dir "/" "" [] 0]
        filledTree = propagateSize bareTree 0
        sizes = map (\(Dir name parentName files filesize) -> filesize) filledTree
        smallDirSum = sum $ filter (< 100000) sizes
        maxSize = maximum sizes
        missingFreeSpace = 30000000 - (70000000 - maxSize)
        gtSize = filter (missingFreeSpace < ) sizes

    print $ show smallDirSum
    print $ show $ minimum gtSize

