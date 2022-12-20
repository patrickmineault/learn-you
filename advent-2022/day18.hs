import Data.List
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP as P
import qualified Data.Map as Map

parse = P.readP_to_S

type Coord = (Int, Int, Int)

parseCoord :: ReadP Coord
parseCoord = do
    x <- read <$> P.munch1 isDigit
    P.char ','
    y <- read <$> P.munch1 isDigit
    P.char ','
    z <- read <$> P.munch1 isDigit
    return (x, y, z)

parseInput :: ReadP [Coord]
parseInput = P.sepBy parseCoord (P.char '\n')

sharesOne :: Coord -> Coord -> Int
sharesOne (x0, y0, z0) (x1, y1, z1) = n
    where 
        does = ((abs(x0 - x1) == 1 && y0 == y1 && z0 == z1) || 
                (abs(y0 - y1) == 1 && x0 == x1 && z0 == z1) || 
                (abs(z0 - z1) == 1 && x0 == x1 && y0 == y1))
        n = if does then 1 else 0        

countCommon :: Coord -> [Coord] -> Int
countCommon a as = 2 * (sum $ map (sharesOne a) as)

countFaces :: [Coord] -> Int
countFaces [] = 0
countFaces (a:as) = 6 - (countCommon a (takeWhile (\(x, _, _) -> x <= x0 + 1) as)) + countFaces as
    where
        (x0, _, _) = a

third :: Coord -> Int
third (_, _, a) = a

floodFill :: Coord -> Coord -> Map.Map Coord Bool -> Map.Map Coord Bool -> Map.Map Coord Bool
floodFill (x0, y0, z0) (x1, y1, z1) blacklist whitelist = newList
    where
        stack = map fst $ Map.toList whitelist
        isValid = \(x2, y2, z2) -> x2 >= x0 && y2 >= y0 && z2 >= z0 && x2 <= x1 && y2 <= y1 && z2 <= z1 && (not $ Map.member (x2, y2, z2) blacklist)
        newStack = stack ++ (concat $ map (\(x, y, z) -> filter isValid [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1)]) stack)
        whitelist2 = (Map.fromList $ zip newStack (repeat True))
        newList = if length whitelist2 > length whitelist then floodFill (x0, y0, z0) (x1, y1, z1) blacklist whitelist2
                    else whitelist2

findInnerPoints :: [Coord] -> [Coord]
findInnerPoints coords = blockedCoords
    where
        (x0', y0', z0') = foldl (\(x0, y0, z0) (x1, y1, z1) -> (min x0 x1, min y0 y1, min z0 z1)) (100, 100, 100) coords
        (x1', y1', z1') = foldl (\(x0, y0, z0) (x1, y1, z1) -> (max x0 x1, max y0 y1, max z0 z1)) (-100, -100, -100) coords
        (x0, y0, z0) = (x0' - 1, y0' - 1, z0' - 1)
        (x1, y1, z1) = (x1' + 1, y1' + 1, z1' + 1)

        globalMap = Map.fromList $ zip coords (repeat True)
        outside = floodFill (x0, y0, z0) (x1, y1, z1) globalMap (Map.fromList [((x0, y0, z0), True)])

        blockedCoords = [(x, y, z) | x <- [x0..x1], y <- [y0..y1], z <- [z0..z1], 
                        not ( Map.member (x, y, z) outside ), 
                        not ( Map.member (x, y, z) globalMap )]

main :: IO ()
main = do
    f <- readFile "data/day18.txt"
    let coords = sort $ fst $ last $ parse parseInput f
        faces = countFaces coords
        blockedCoords = findInnerPoints coords
        residualFaces = faces - (countFaces blockedCoords)
    print $ show faces
    print $ show residualFaces
