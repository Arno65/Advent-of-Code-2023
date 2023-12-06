-- Advent of Code 2023 - Day 6 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The product of all winings is:           1624896
-- The product of the big race winings is: 32583852
--
-- (cl) by Arno Jacobs, 2023-12-06

module AoC2023d06ab where

import Data.Char

-- Some initials
filename :: String
filename = "data/inputDay06_2023.txt"

-- Some helpers 
-- Just liked to parse the race data

-- Convert a string with a list of integers to a list of (positive only) integers
getInts :: String -> [Int]
getInts []      = []
getInts line    | isDigit (head line)   = [value] ++ getInts restLine
                | otherwise             = getInts (tail line)
    where
        value       = read $ takeWhile isDigit line
        restLine    = dropWhile isDigit line

inLine :: String -> String -> Bool
inLine token line = or [ linePart == token |  
                            lix <- [0..lpl], 
                            let linePart = take tln (drop lix line) ]
    where
        tln = length token
        lpl = length line - tln

parseInts :: String -> [String] -> [Int]
parseInts token = concat . map getInts . filter (inLine token)

-- Part 1
travelDistance :: Int -> Int -> Int
travelDistance totalTime chargeTime = chargeTime * (max 0 (totalTime - chargeTime))

countWins :: (Int,Int) -> Int
countWins ( time, distance ) = sum [ 1 |    chargeTime <- [1..time-1],
                                            distance < travelDistance time chargeTime ]

productAllWinnings :: [Int] -> [Int] -> Int
productAllWinnings time = product . map countWins . zip time 

-- part 2
parseBigInt :: String -> [String] -> [Int]
parseBigInt token = getInts . concat . map (filter isDigit) . filter (inLine token)


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 6  (Haskell)"
            raceData <- lines <$> readFile filename

            let time        = parseInts "Time"      raceData
            let distance    = parseInts "Distance"  raceData
            putStr "The product of all winings is:           "
            print $ productAllWinnings time distance

            let bigTime     = parseBigInt "Time"     raceData
            let bigDistance = parseBigInt "Distance" raceData
            putStr "The product of the big race winings is: "
            print $ productAllWinnings bigTime bigDistance
            
            putStrLn "0K.\n"
