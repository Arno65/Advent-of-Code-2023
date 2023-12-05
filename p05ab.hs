-- Advent of Code 2023 - Day 5 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The lowest location number that corresponds to any of the initial seed numbers: 
-- Part 1: 165788812
-- Part 2:   1928058
--
-- (cl) by Arno Jacobs, 2023-12-05

-- A compiled version with the Not-So-Intelligent brute force solution needs 8+ minutes to run
-- Brute force as finding ALL locations for ALL seeds
--
-- I haven't found a more efficient smarter solution yet :-(


module AoC2023d05ab where

import Data.Char
import Data.List.Split


-- Some initials
filename :: String
filename = "data/inputDay05_2023.txt"

cSpace = ' ' :: Char

type Mapping        = (Int,Int,Int)
type MappingList    = [Mapping]

-- First the parsing of the data

-- Convert a string with a list of integers 
-- to a list of integers
-- Be aware: this is NO error checking
getInts :: String -> [Int]
getInts = map read . (splitOn [cSpace])

-- Check if string starts with a digit
-- Skipping leading spaces
--    
digitStart :: String -> Bool
digitStart xs   | xs' == ""             = False
                | isDigit (head xs')    = True
                | otherwise             = False
    where
        xs' = dropWhile (\c -> c == cSpace) xs

-- Safe method for list length < 3
listToMap :: [Int] -> Mapping
listToMap []            = (  0,  0,  0 )
listToMap (e:[])        = (  e,  0,  0 )
listToMap (e1:e2:[])    = ( e1, e2,  0 )
listToMap (e1:e2:e3:_)  = ( e1, e2, e3 )

getMappings :: [String] -> [MappingList]
getMappings []      = []
getMappings almanac 
    | partialMappings == [] = getMappings remainingAlmanac
    | otherwise             = [partialMappings] ++ getMappings remainingAlmanac
        where
            ( partialMappings, remainingAlmanac ) = parsePartialMappings [] almanac
        
parsePartialMappings :: MappingList -> [String] -> (MappingList,[String])
parsePartialMappings mappingsList []                        = ( mappingsList, [] )
parsePartialMappings mappingsList (map:remainingAlmanac)
    | digitStart map    = parsePartialMappings newMappingList remainingAlmanac
    | otherwise         = ( mappingsList, remainingAlmanac )
        where
            newMap          = listToMap $ getInts map  
            newMappingList  = mappingsList ++ [newMap] 

-- Get almanac input and parse to
-- 1. list of seeds
-- 2. list of list of mappings
-- 
parseAlmanac :: [String] -> ([Int],[MappingList])
parseAlmanac (seedsList:remainingAlmanac) = ( seeds, mappingsList )
    where
        seeds           = getInts $ dropWhile (not . isDigit) seedsList
        mappingsList    = getMappings $ dropWhile (not . digitStart) remainingAlmanac

-- Part 1

-- Seed to location

seedToLocation :: [MappingList] -> Int -> Int
seedToLocation []             ix    = ix
seedToLocation (map:mappings) ix    = seedToLocation mappings nextStep
    where
        nextStep = mapsStep map ix

mapsStep :: MappingList -> Int -> Int
mapsStep []                          ix = ix        
mapsStep (mapping:remainingMappings) ix 
    | maybeStep == Nothing  = mapsStep remainingMappings ix
    | otherwise             = nextStep 
        where
            maybeStep       = mapStep mapping ix
            Just nextStep   = maybeStep

mapStep :: Mapping -> Int -> Maybe Int
mapStep ( source, destination, range ) ix
    |   ix >= destination 
    &&  ix <  (destination + range) = Just (source - destination + ix)
    |   otherwise                   = Nothing

lowestLocation :: [MappingList] -> [Int] -> Int
lowestLocation mappings = minimum . map (seedToLocation mappings)

-- Part 2 

lowestLocationOnRange :: [MappingList] -> [Int] -> Int 
lowestLocationOnRange mappings seeds = lowestLocationOnRange' mappings seeds []

lowestLocationOnRange' :: [MappingList] -> [Int] -> [Int] -> Int 
lowestLocationOnRange' mappings []                                    locations = minimum locations
lowestLocationOnRange' mappings (startSeed:range:remainingSeedRanges) locations =
    lowestLocationOnRange' mappings remainingSeedRanges newLocation
        where
            seeds           = [ startSeed .. startSeed + range - 1 ]
            lowestInRange   = lowestLocation mappings seeds
            newLocation     = locations ++ [lowestInRange]


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 5  (Haskell)"
            ( seeds, mappings ) <- parseAlmanac <$> lines <$> readFile filename
            putStrLn "The lowest location number that corresponds"
            putStrLn "to any of the initial seed numbers"
            putStr " Part 1: "
            print $ lowestLocation mappings seeds

            putStr " Part 2:   "
-- This one will take 8+ minutes on my MacBook Air M1 (the M2 is 10% faster)
            print $ lowestLocationOnRange mappings seeds

            putStrLn "0K.\n"

