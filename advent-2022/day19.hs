import Data.List
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP as P
import qualified Data.Map as Map

parse = P.readP_to_S

type ResourceHistory = [Int]
type Resources = (Int, Int, Int, Int)

type Bot = Int

data PartialSequence = PartialSequence {
    resources :: Resources,
    bots :: Resources,
    plan :: Bot
} deriving Show

type CostTable = Map.Map Bot Resources

forkSequence :: PartialSequence -> Bot -> PartialSequence
forkSequence seq bot = newSeq
    where
        newSeq = PartialSequence (resources seq) (bots seq) bot

advanceSequence :: CostTable -> PartialSequence -> [PartialSequence]
advanceSequence costTable seq = newSeqs
    where
        (ore, clay, obsidian, geode) = resources seq
        (oreP, clayP, obsidianP, geodeP) = bots seq

        (oreC, clayC, obsidianC, geodeC) = costTable Map.! (plan seq)
        
        --Do I have enough to buy? Then buy
        canAfford = ore >= oreC && clay >= clayC && obsidian >= obsidianC && geode >= geodeC
        resourcesNext = if canAfford then (ore - oreC + oreP, clay - clayC + clayP, obsidian - obsidianC + obsidianP, geode - geodeC + geodeP)
                        else (ore + oreP, clay + clayP, obsidian + obsidianP, geode + geodeP)

        botsNext = if canAfford then case plan seq of
            1 -> (oreP + 1, clayP, obsidianP, geodeP)
            2 -> (oreP, clayP + 1, obsidianP, geodeP)
            3 -> (oreP, clayP, obsidianP + 1, geodeP)
            4 -> (oreP, clayP, obsidianP, geodeP + 1)
         else (oreP, clayP, obsidianP, geodeP)

        (oreR, clayR, obsidianR, geodeR) = botsNext

        newSeq = PartialSequence resourcesNext botsNext (plan seq)
        newSeqs
            | not canAfford  = [newSeq]
            | clayR == 0     = [forkSequence newSeq 1, forkSequence newSeq 2]
            | obsidianR == 0 = [forkSequence newSeq 1, forkSequence newSeq 2, forkSequence newSeq 3]
            | otherwise      = [forkSequence newSeq 1, forkSequence newSeq 2, forkSequence newSeq 3, forkSequence newSeq 4]

evalOneStage :: Int -> CostTable -> [PartialSequence] -> [PartialSequence]
evalOneStage iteration costTable seqs = cull iteration costTable $ concat $ map (advanceSequence costTable) seqs

nbest = 1

cull :: Int -> CostTable -> [PartialSequence] -> [PartialSequence]
cull iteration costTable newSeqs
    | length newSeqs < nbest = newSeqs
    | otherwise = thebest
    where
        geodesNow = map ((\(_, _, _, a) -> a) . (\x -> resources x)) newSeqs
        geodesThen = map ((\(_, _, _, a) -> a * iteration) . (\x -> bots x)) newSeqs
        scores = zipWith (+) geodesNow geodesThen
        threshScore = (sort scores) !! ((length scores) - nbest)
        thebest = map snd $ filter (\(x, y) -> x >= threshScore) $ zip scores newSeqs

--Count the number of geodes at the end
evaluatePlan :: CostTable -> Int
evaluatePlan costTable = geodes
    where
        seqs = [PartialSequence (0, 0, 0, 0) (1, 0, 0, 0) 1,
                PartialSequence (0, 0, 0, 0) (1, 0, 0, 0) 2]
        allEvals = foldl (\x y -> evalOneStage (24 - y) costTable x) seqs [1..24]
        geodes = maximum $ map ((\(_, _, _, a) -> a) . (\x -> resources x)) allEvals

parseTable :: ReadP CostTable
parseTable = do
    P.string "Blueprint "
    P.munch1 isDigit
    P.string ": Each ore robot costs "
    ore <- read <$> P.munch1 isDigit
    P.string " ore. Each clay robot costs "
    clay <- read <$> P.munch1 isDigit
    P.string " ore. Each obsidian robot costs "
    oreObs <- read <$> P.munch1 isDigit
    P.string " ore and "
    clayObs <- read <$> P.munch1 isDigit
    P.string " clay. Each geode robot costs "
    oreGeode <- read <$> P.munch1 isDigit
    P.string " ore and "
    obsGeode <- read <$> P.munch1 isDigit
    P.string " obsidian."

    return (Map.fromList [(1, (ore, 0, 0, 0)),
                          (2, (clay, 0, 0, 0)),
                          (3, (oreObs, clayObs, 0, 0)),
                          (4, (oreGeode, 0, obsGeode, 0))])

parseInput :: ReadP [CostTable]
parseInput = P.sepBy parseTable (P.char '\n')

main :: IO ()
main = do
    f <- readFile "data/day19.txt"
    let costTables = fst $ last $ parse parseInput f
        evaluations = map evaluatePlan costTables
        qualityLevels = zipWith (*) [1..] evaluations
    print $ show costTables
    print $ show evaluations
    print $ show $ sum qualityLevels