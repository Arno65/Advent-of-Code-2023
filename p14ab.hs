-- Advent of Code 2023 - Day 14 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The total load on the north support beams: 109596
-- The total load after 1000000000 cycles:     96105
--
-- (cl) by Arno Jacobs, 2023-12-14

module AoC2023d14ab where

import Data.List

cFreeSpace      = '.' :: Char
cRollingStone   = 'O' :: Char
cSteadyRock     = '#' :: Char

megaCycles      = 1000000000 :: Int

-- Some initials
filename :: String
filename = "data/inputDay14_2023.txt"


-- Part 1

rollLeft :: String -> String
rollLeft = concat . map rollPartToLeft . seperateOn cSteadyRock

rollRight :: String -> String
rollRight = reverse . rollLeft . reverse

rollPartToLeft :: String -> String
rollPartToLeft xs   
    | elem cSteadyRock xs   = xs
    | otherwise             = theRollingStones ++ theFreeSpace
        where
            theRollingStones = filter (== cRollingStone) xs
            theFreeSpace     = filter (== cFreeSpace) xs

-- 
seperateOn :: Eq a => a -> [a] -> [[a]]
seperateOn _   []       = []
seperateOn sep xs 
    | head xs == sep    = [seperators] ++ seperateOn sep afterSeperators
    | otherwise         = [rest]       ++ seperateOn sep afterRest
        where
            seperators      = takeWhile (== sep) xs
            afterSeperators = dropWhile (== sep) xs 
            rest            = takeWhile (/= sep) xs
            afterRest       = dropWhile (/= sep) xs

-- Calculate the load of the slided rocks - line by line 
calculateLoad :: String -> Int
calculateLoad xs = sum . map fst . filter ((== cRollingStone) . snd) $ zip [1..] xs 
    
-- Transpose the dish data - so the rolling stones will roll to the "west" (left)
-- Flip dish by reverse for easy indexing and calculating load
totalLoad :: [String] -> Int
totalLoad = sum . map (calculateLoad . reverse . rollLeft) . transpose


-- Part 2

-- One cycle: North -> West -> South -> East
--
-- North:  T                 roll                 T
-- West:                     roll
-- South:  T  map (reverse)  roll  map (reverse)  T
-- East:      map (reverse)  roll  map (reverse)

rollNorth :: [String] -> [String]
rollNorth = transpose . map rollLeft . transpose

rollWest :: [String] -> [String]
rollWest = map rollLeft

rollSouth :: [String] -> [String]
rollSouth = transpose . map rollRight . transpose

rollEast :: [String] -> [String]
rollEast = map rollRight

oneCycle :: [String] -> [String]
oneCycle = rollEast . rollSouth . rollWest . rollNorth

nCycles :: Int -> [String] -> [String]
nCycles count dish  | count < 2 = oneCycle dish 
                    | otherwise = nCycles (count - 1) (oneCycle dish)

getModuloCycle :: [String] -> [String] -> Int
getModuloCycle firstCycle currentCycle 
    | nextCycle == firstCycle   = 1
    | otherwise                 = 1 + getModuloCycle firstCycle nextCycle 
        where
            nextCycle = oneCycle currentCycle

totalLoadAfterCycle :: [String] -> Int
totalLoadAfterCycle = sum . map (calculateLoad . reverse) . transpose

workToPattern :: [String] -> [[String]] -> Int -> (Int,[String])
workToPattern dish previousDishes cycles    
    | elem dish previousDishes  = (cycles,dish)
    | otherwise                 = workToPattern nextDish currentDishes (cycles+1)
        where
            nextDish        = oneCycle dish
            currentDishes   = [dish] ++ previousDishes

workMegaCycles ::  Int -> [String] -> Int
workMegaCycles cycles dish = totalLoadAfterCycle $ nCycles remainingCycles dish' 
    where
        (startCycles,dish') = workToPattern dish [] 0
        moduloCycles        = getModuloCycle dish' dish'
        remainingCycles     = mod (cycles - startCycles) moduloCycles

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 14  (Haskell)"
            dish <- lines <$> readFile filename            

            putStr "The total load on the north support beams: "
            print $ totalLoad dish
            putStr "The total load after "
            putStr $ show megaCycles
            putStr " cycles:     "
            print $ workMegaCycles megaCycles dish

            putStrLn "0K.\n"

