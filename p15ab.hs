-- Advent of Code 2023 - Day 15 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The Holiday ASCII String Helper sum of results:         511343
-- The focusing power of the resulting lens configuration: 294474
--
-- (cl) by Arno Jacobs, 2023-12-15

module AoC2023d15ab where

import Data.List.Split
import Data.Char

data Operation = PlaceLens | RemoveLens         deriving (Eq,Show)

data HASH = HASH    {   operation   :: Operation,
                        label       :: String,
                        box         :: Int,
                        focalLength :: Int  }   deriving (Eq,Show)

-- Some initials
filename :: String
filename = "data/inputDay15_2023.txt"

sComma  = "," :: String
sIs     = "=" :: String
cMinus  = '-' :: Char


-- Part 1

workHASH :: [String] -> Int
workHASH = sum . map (foldl (\cv ac -> (17 * (ord ac + cv)) `mod` 256) 0)


-- Part 2

parseWord :: String -> HASH
parseWord xs    | length command < 2 = HASH {   operation   = RemoveLens,
                                                label       = init label,
                                                box         = box,
                                                focalLength = 0 }                                                 
                | otherwise          = HASH {   operation   = PlaceLens,
                                                label       = label,
                                                box         = box,
                                                focalLength = focalLength }
    where
        command     = splitOn sIs xs
        label       = head command
        box         = workHASH [label]
        focalLength = read $ last command

workLenses :: [String] -> Int
workLenses = sum . map focusingPower . addSlotNumbers . configureLenses []

configureLenses :: [HASH] -> [String] -> [HASH]
configureLenses boxes []        = boxes
configureLenses boxes (oc:ocs) 
    | todo == PlaceLens = configureLenses updatedBoxes ocs
    | otherwise         = configureLenses cleanedBoxes ocs
    where
        hash         = parseWord oc
        todo         = operation hash
        updatedBoxes = updateBox boxes hash
        cleanedBoxes = removeBox boxes hash

updateBox :: [HASH] -> HASH -> [HASH]
updateBox []            hash    = [hash]
updateBox (box:rboxes)  hash    
    | label box == label hash   =  [hash] ++ rboxes
    | otherwise                 =  [box]  ++ updateBox rboxes hash

removeBox :: [HASH] -> HASH -> [HASH]
removeBox []    _       = []
removeBox boxes hash    = filter (\b -> label b /= label hash) boxes

addSlotNumbers :: [HASH] -> [(HASH,Int)]
addSlotNumbers []       = []
addSlotNumbers hashes   = zip slotHash [1..] ++ addSlotNumbers nextHashes
    where
        boxNumber   = box $ head hashes
        slotHash    = filter (hasBoxNumber boxNumber)         hashes
        nextHashes  = filter (not . (hasBoxNumber boxNumber)) hashes
        --
        hasBoxNumber bn hash = bn == box hash

focusingPower :: (HASH,Int) -> Int
focusingPower (hash,slotNumber) = (1 + box hash) * (focalLength hash) * slotNumber

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 15 (Haskell)"
            words <- (splitOn sComma) <$> readFile filename            
            putStr "The Holiday ASCII String Helper sum of results:         "
            print $ workHASH words
            putStr "The focusing power of the resulting lens configuration: "
            print $ workLenses  words
            putStrLn "0K.\n"

