import Data.Char
import qualified Data.Map as Map
import Control.Lens.Indexed

type Coord = (Int, Int)
data Role = Start | End | Other deriving Show


data Node  = Node {
    coords :: Coord
  , role :: Role
  , height :: Int
  , adjacent :: [Coord]
} deriving Show

doOneStep :: Map.Map Coord Node -> Map.Map Coord Node -> [Node] -> Int -> Int
doOneStep theMap visited [] num = num
doOneStep theMap visited stack num
    | num == 1 = num
    | otherwise = 1 + done
    where
        done = 

bfs :: Map.Map Coord Node -> Int 
bfs theMap = doOneStep theMap visited [start]
    where
        theList = filter (\(k, v) -> role v == Start) (Map.fromList theMap)
        start = theList !! start
        visited = Map.empty :: Map.Map Coord Node

findAdjacent :: Int -> Map.Map Coord (Role, Int) -> [Coord] -> [Coord]
findAdjacent height theMap coords = adjacents
    where
        existing = filter (\x -> Map.member x theMap) coords
        isAdjacent = map (\x -> height >= -1 + (snd (theMap Map.! x))) existing
        adjacents = map snd $ filter fst (zip isAdjacent existing)

rawToNodes :: Map.Map Coord (Role, Int) -> Map.Map Coord Node
rawToNodes theMap = nodes
    where
        theList = Map.toList theMap
        toNodes = (\((x, y), (role, height)) -> 
            ((x, y), Node role height (findAdjacent height theMap [(x, y-1), (x, y+1), (x-1, y), (x+1, y)])))
        nodeList = map toNodes theList
        nodes = Map.fromList nodeList
        

charToHeight :: Char -> (Role, Int)
charToHeight 'S' = (Start, 0)
charToHeight 'E' = (End, 25)
charToHeight a = (Other, (ord a) - (ord 'a'))

toHmap :: [String] -> Map.Map Coord (Role, Int)
toHmap lines = theMap
    where
        heightList = concat $ map (map charToHeight) lines
        coords = [(x, y) | y <- [0..((length lines)-1)], x <- [0..((length (lines !! 0)) - 1)]]
        theMap = Map.fromList $ zip coords heightList

main :: IO ()
main = do
    heightMap <- readFile "data/day12_test.txt"
    let heights = lines heightMap
        heightHmap = toHmap heights 
        nodes = rawToNodes heightHmap
        best = bfs nodes
    print $ show nodes