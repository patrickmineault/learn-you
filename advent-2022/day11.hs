import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.List
import Text.ParserCombinators.ReadP as P

data Monkey = Monkey {
    number :: Int
  , items :: [Int]
  , operation :: Int -> Int
  , test :: Int
  , whenTrue :: Int
  , whenFalse :: Int
  , inspections :: Int
  }

safemax :: [Int] -> Int
safemax [] = 0
safemax a = maximum a

instance Show Monkey where 
  show m = "Monkey(" ++ (show $ number m) ++ ", nitems=" ++ (show $ length (items m)) ++ ", inspections=" ++ (show $ inspections m) ++ ", maxitems=" ++ (show $ safemax $ items m) ++ ")"

parseOp :: ReadP (Int -> Int -> Int)
parseOp = readAdd <|> readMult
  where
    readAdd = P.char '+' $> (+)
    readMult = P.char '*' $> (*)

parseItems :: ReadP [Int]
parseItems = do
  P.string "Starting items: "
  map read <$> P.sepBy (P.munch1 isDigit) (P.string ", ")

parseOperation :: ReadP (Int -> Int)
parseOperation = do
  P.string "Operation: new = old "
  op <- parseOp
  P.skipSpaces
  right <- P.string "old" <|> P.munch1 isDigit
  case right of
    "old" -> return (\n -> op n n)
    d -> return (op (read d))

parseTest :: ReadP Int
parseTest = do
  P.string "Test: divisible by "
  read <$> P.munch1 isDigit

parseWhenTrue :: ReadP Int
parseWhenTrue = do
  P.string "If true: throw to monkey "
  read <$> P.munch1 isDigit

parseWhenFalse :: ReadP Int
parseWhenFalse = do
  P.string "If false: throw to monkey "
  read <$> P.munch1 isDigit

parseMonkey :: ReadP Monkey
parseMonkey = do
  P.string "Monkey "
  number <- read <$> P.munch1 isDigit
  P.char ':'
  P.skipSpaces
  items <- parseItems
  P.skipSpaces
  operation <- parseOperation
  P.skipSpaces
  test <- parseTest
  P.skipSpaces
  whenTrue <- parseWhenTrue
  P.skipSpaces
  whenFalse <- parseWhenFalse
  return (Monkey number items operation test whenTrue whenFalse 0)

parseInput :: ReadP [Monkey]
parseInput = P.sepBy parseMonkey P.skipSpaces

parse = P.readP_to_S

div3 :: Int -> Int
div3 a = div a 3

splice :: Monkey -> Int -> [Monkey] -> [Monkey]
splice item num items = (take num items) ++ [item] ++ (drop (num + 1) items)

doOneStep :: (Int -> Int) -> [Monkey] -> Int ->[Monkey]
doOneStep op ms num = m2
  where
    m = ms !! num
    newItems = map (op . (operation m)) (items m)
    throwTrue = filter (\x -> mod x (test m) == 0) newItems
    throwFalse = filter (\x -> mod x (test m) /= 0) newItems
    m' = Monkey (number m) ([]) (operation m) (test m) (whenTrue m) (whenFalse m) ((length newItems) + inspections m)
    mT = ms !! (whenTrue m)
    mF = ms !! (whenFalse m)
    mT' = Monkey (number mT) ((items mT) ++ throwTrue) (operation mT) (test mT) (whenTrue mT) (whenFalse mT) (inspections mT)
    mF' = Monkey (number mF) ((items mF) ++ throwFalse) (operation mF) (test mF) (whenTrue mF) (whenFalse mF) (inspections mF)
    m2 = splice m' (number m') $ splice mT' (number mT') $ splice mF' (number mF') ms

doMultipleRounds :: (Int -> Int) -> [Monkey] -> Int -> [Monkey]
doMultipleRounds op ms num = ms'
  where
    nums = [a `mod` (length ms) | a <- [0..(num * (length ms) -1)]]
    ms' = foldl (doOneStep op) ms nums 

main :: IO ()
main = do
  f <- readFile "data/day11.txt"
  let monkeys = fst $ last $ parse parseInput f
      monkeys' = doMultipleRounds div3 monkeys 20
      factor = foldl (*) 1 $ map test monkeys
      modFactor = \x -> x `mod` factor
      ms2 = doMultipleRounds modFactor monkeys 10000
      sorted = sort $ map inspections ms2
  print $ show $ monkeys'
  print $ show $ (foldl (*) 1 $ drop 6 sorted)