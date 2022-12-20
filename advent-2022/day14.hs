import Data.Char (isDigit)
import Text.ParserCombinators.ReadP as P
import qualified Data.HashMap as Map

data Material = S | R deriving (Show, Eq)

type Coord = (Int, Int)
type Rock = [Coord]
type StopCondition = Coord -> Bool

parseCoord :: ReadP Coord
parseCoord = do
    x <- read <$> P.munch1 isDigit 
    P.char ','
    y <- read <$> P.munch1 isDigit
    return (x, y)

parseRock :: ReadP Rock
parseRock = P.sepBy parseCoord (P.string " -> ")

parseInput :: ReadP [Rock]
parseInput = P.sepBy parseRock (P.char '\n')

parse = P.readP_to_S

drawSegment :: Coord -> Coord -> [(Coord, Material)]
drawSegment (x0, y0) (x1, y1)
    | (x0 == x1) && (y1 >= y0) = [((x0, y), R) | y <- [y0..y1]]
    | (x0 == x1) && (y0 > y1) = [((x0, y), R) | y <- [y1..y0]]
    | (y0 == y1) && (x1 >= x0) = [((x, y0), R) | x <- [x0..x1]]
    | (y0 == y1) && (x0 > x1) = [((x, y0), R) | x <- [x1..x0]]

drawRockLine :: Rock -> [(Coord, Material)]
drawRockLine [] = []
drawRockLine [a] = []
drawRockLine [a,b] = drawSegment a b
drawRockLine (a:b:rest) = (drawSegment a b) ++ (drawRockLine (b:rest))

drawField :: [Rock] -> Map.Map Coord Material
drawField rocks = Map.fromList $ concat $ map drawRockLine rocks

placeGrain :: StopCondition -> Map.Map Coord Material -> Coord -> Maybe Coord
placeGrain stop theMap (x, y)
    | stop (x, y) = Nothing -- Grain of sand has fallen out of the bottom
    | not (Map.member (x, y + 1) theMap) = placeGrain stop theMap (x, y + 1)
    | not (Map.member (x - 1, y + 1) theMap) = placeGrain stop theMap (x - 1, y + 1)
    | not (Map.member (x + 1, y + 1) theMap) = placeGrain stop theMap (x + 1, y + 1)
    | y == 0 = Nothing -- Field is full
    | otherwise = Just (x, y) -- Sand grain has settled.

dropSand :: StopCondition -> Map.Map Coord Material -> Map.Map Coord Material
dropSand stop field = next
    where
        newCoord = placeGrain stop field (500, 0)
        next = case newCoord of
            Nothing -> field
            Just c -> dropSand stop (Map.insert c S field)

main :: IO ()
main = do
    f <- readFile "data/day14.txt"
    let coords = fst $ last $ parse parseInput f
        bottom = maximum $ concat $ map (map snd) coords
        field = drawField coords
        droppedField = dropSand (\(x, y) -> y > bottom) field
        sandGrains = length $ filter (\(coord, role) -> role == S) (Map.toList droppedField)

        field2 = drawField (coords ++ [[(0, bottom + 2), (1000, bottom + 2)]])
        droppedField2 = dropSand (\(x, y) -> False) field2
        sandGrains2 = 1 + (length $ filter (\(coord, role) -> role == S) (Map.toList droppedField2))

    print $ show sandGrains
    print $ show sandGrains2
