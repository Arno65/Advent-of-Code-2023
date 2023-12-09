-- Advent of Code 2023 - Day 9 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of these extrapolated future values: 1939607039
-- The sum of these extrapolated past values:         1041

--
-- (cl) by Arno Jacobs, 2023-12-09

module AoC2023d09ab where

import Data.List.Split

-- Some initials
filename :: String
filename = "data/inputDay09_2023.txt"

data Time = Past | Future deriving (Eq,Show)

sSpace = " " :: String

getInts :: String -> [Int]
getInts = map read . splitOn sSpace

-- Part 1

deltas :: [Int] -> [Int]
deltas xs = map (\p -> snd p - fst p) $ zip xs (tail xs)
        
-- Keep every first or last value of the list at each delta function
-- dieection is either Past or Future
allDeltasZeros :: Time -> [Int] -> [Int] -> [Int]
allDeltasZeros direction savedValues values 
    | allZeros && direction == Past     = newFirstValues
    | allZeros && direction == Future   = newFirstValues
    | direction == Past                 = allDeltasZeros direction newFirstValues newValues 
    | otherwise                         = allDeltasZeros direction newLastValues  newValues
        where
            newValues       = deltas values
            newFirstValues  = [head values] ++ savedValues
            newLastValues   = [last values] ++ savedValues
            allZeros        = [] == filter (/=0) newValues 

calculateToFuture :: [Int] -> Int
calculateToFuture = sum . (allDeltasZeros Future [])

workToFuture :: [[Int]] -> Int
workToFuture = sum . map calculateToFuture

-- Part 2

-- "Je gaat het pas zien als je het door hebt." J.C.
--
calculateToPast :: [Int] -> Int
calculateToPast = subtracts . (allDeltasZeros Past [])
    where
        subtracts = foldl subtract 0

workToPast :: [[Int]] -> Int
workToPast = sum . map calculateToPast

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 9  (Haskell)"
            histories <- map getInts <$> lines <$> readFile filename
            putStr   "The sum of these extrapolated future values: "
            print $ workToFuture histories
            putStr   "The sum of these extrapolated past values:         "
            print $ workToPast histories
            putStrLn "0K.\n"

