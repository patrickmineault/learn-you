#!/usr/bin/env cabal
{- cabal:
build-depends: base, tasty-hunit, tasty, containers
-}

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

findCommon :: [String] -> Char
findCommon (a:b:c) = let set1 = Set.fromList a
                         set2 = Set.fromList b
                         set3 = Set.fromList $ c !! 0
                         common = Set.intersection (Set.intersection set1 set2) set3
                     in Set.findMin common

findMisplaced :: String -> Char
findMisplaced a = let split = splitAt (length a `div` 2) a
                      set1 = Set.fromList $ fst split
                      set2 = Set.fromList $ snd split
                      misplaced = Set.intersection set1 set2
                  in Set.findMin misplaced

scoreLetter :: Char -> Int
scoreLetter a
    | isUpper a = ord a - (ord 'A') + 27
    | otherwise = ord a - (ord 'a') + 1

testMisplaced :: TestTree
testMisplaced = testCase "Should work" $ do
    f <- readFile "data/day3_test.txt"
    let theLines = lines f
        misplaced = map findMisplaced theLines
    misplaced @?= "pLPvts"

test :: IO()
test = defaultMain $ testGroup "Tests" [testMisplaced]

main :: IO()
main = do 
    f <- readFile "data/day3.txt"
    let theLines = lines f
        scores = map (scoreLetter . findMisplaced) theLines
        totalScore = sum scores
    print $ show totalScore

    let elvePacks = chunksOf 3 (lines f)
        letterInCommon = map (scoreLetter . findCommon) elvePacks 
        commonScore = sum letterInCommon

    print $ show commonScore

