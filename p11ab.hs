-- Advent of Code 2023 - Day 11 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of distances between every pair of galaxies (part 1):       940302
-- The sum of distances between every pair of galaxies (part 2): 543018317006
--
-- (cl) by Arno Jacobs, 2023-12-11

-- 
module AoC2023d11ab where

import Data.List

type Universe = [String]
type Position = (Int,Int)


cGalaxy             = '#'       :: Char
cExpansion          = '0'       :: Char
expansionSizePart1  = 2         :: Int
expansionSizePart2  = 1000000   :: Int

-- Some initials
filename :: String
filename = "data/inputDay11_2023.txt"

-- The Manhattan distance between two points
manhattan :: Position -> Position -> Int
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

getGridRange :: [[a]] -> (Int,Int)
getGridRange grid = (mx,my)
    where
        mx  = length $ head grid
        my  = length grid

-- Part 1 and 2

expandUniverse :: Universe -> Universe
expandUniverse = transpose . megaExpandRows . transpose . megaExpandRows
    where
        megaExpandRows []       = []
        megaExpandRows (r:rs)   | elem cGalaxy r = [r]    ++ megaExpandRows rs
                                | otherwise      = [mega] ++ megaExpandRows rs
            where
                mega = replicate (length r) '0'

scanGalaxyPositions :: Int -> Universe -> [Position]
scanGalaxyPositions expansion universe = 
    [ (x + expansionX, y + expansionY ) 
            |   x <- [0..maxX-1], 
                y <- [0..maxY-1],
                universe !! y !! x == cGalaxy,
                let expansionX = expand x bigStepsX,
                let expansionY = expand y bigStepsY ]
        where
            (maxX,maxY) = getGridRange universe
            bigStepsX = selectExpansion $ zip (head universe) [0..]
            bigStepsY = selectExpansion $ zip ((head . transpose) universe) [0..]
            selectExpansion = map snd . filter ((== cExpansion) . fst)
            expand coor steps = (expansion - 1) * length (filter (< coor) steps)

calculateAllShortestDistances :: [Position] -> Int
calculateAllShortestDistances []        = 0
calculateAllShortestDistances (g:gs)    = sumDX + calculateAllShortestDistances gs
    where
        sumDX = sum [ manhattan g g' | g' <- gs ]

getDistances :: Int -> Universe -> Int 
getDistances expansion =    calculateAllShortestDistances . 
                            (scanGalaxyPositions expansion) . 
                            expandUniverse

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 11 (Haskell)"
            flatWorld <- lines <$> readFile filename
            putStr   "The sum of the lengths (part 1):      "
            print $ getDistances expansionSizePart1 flatWorld
            putStr   "The sum of the lengths (part 2): "
            print $ getDistances expansionSizePart2 flatWorld
            putStrLn "0K.\n"

