-- Advent of Code 2023 - Day 1 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of all of the    calibration values: 54679
-- The sum of all of the re-calibration values: 54885
--
-- (cl) by Arno Jacobs, 2023-12-01

-- 
module AoC2023d01ab where

import Data.List 

-- Some initials
filename :: String
filename = "data/inputDay01_2023.txt"

digits :: String
digits = ['0'..'9']

spelledDigits :: [String]
spelledDigits = [ "one","two","three","four","five","six","seven","eight","nine" ]

indexedSpelledDigits :: [(String,Int)]
indexedSpelledDigits = zip spelledDigits [1..]

-- Part 1

calibrateStep1 :: String -> String
calibrateStep1 []       = []
calibrateStep1 (x:xs)   | elem x digits     = [x] ++ calibrateStep1 xs
                        | otherwise         =        calibrateStep1 xs 

calibrateLine :: String -> Int
calibrateLine line = read $ (head ch):(last ch):[]
    where
        ch = calibrateStep1 line

calibrate :: [String] -> Int 
calibrate = sum . map calibrateLine

-- Part 2

spelledHeadIndex :: [(String,Int)] -> String -> Maybe Int
spelledHeadIndex []              _  = Nothing
spelledHeadIndex ((sd,ix):shixs) xs 
    | sd == take (length sd) xs = Just ix
    | otherwise                 = spelledHeadIndex shixs xs

-- replace spelled digits with number digits
reCalibrateLine :: String -> String
reCalibrateLine []      = []
reCalibrateLine xs      | shh == Nothing    = [head xs]    ++ reCalibrateLine (tail xs)
                        | otherwise         = (show digit) ++ reCalibrateLine (tail xs) 
    where
        shh         = spelledHeadIndex indexedSpelledDigits xs
        Just digit  = shh

reCalibrate :: [String] -> Int
reCalibrate = calibrate . map reCalibrateLine


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 1  (Haskell)"
            day1 <- lines <$> readFile filename

            putStr   "The sum of all of the calibration values:    "
            print $ calibrate day1
            putStr   "The sum of all of the re-calibration values: "
            print $ reCalibrate day1 
            putStrLn "0K.\n"

