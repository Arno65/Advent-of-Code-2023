-- Advent of Code 2023 - Day 3 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of all of the part numbers in the engine schematic:   537732
-- The sum of all of the gear ratios in the engine schematic:  84883664
--
-- (cl) by Arno Jacobs, 2023-12-03

-- 
module AoC2023d03ab where

import Data.Char (isDigit)

type Position = (Int,Int)

-- Some initials
filename :: String
filename = "data/inputDay03_2023.txt"

cDot  = '.' :: Char
cGear = '*' :: Char


-- Extract positive integers (only)
-- The '-' character is a symbol in the dataset
extractInt :: String -> (Int,String)
extractInt []       = (0,[])
extractInt (x:xs)   | isDigit x     = (i,rs)
                    | otherwise     = extractInt xs
    where
        i  = read (x:(takeWhile isDigit xs))
        rs = dropWhile isDigit xs 

-- Add empty safety borders
expandedSchematic :: [String] -> [String]
expandedSchematic schematic = [emptyRow] ++ (map addEmptyBorder schematic) ++ [emptyRow]
    where
        newRowLength        = 2 + length (head schematic)
        emptyRow            = replicate newRowLength cDot 
        expandedSchematic   = [emptyRow] ++ (map addEmptyBorder schematic) ++ [emptyRow]
        addEmptyBorder row  = [cDot] ++ row ++ [cDot]

isSymbol :: Char -> Bool
isSymbol c  | isDigit c = False
            | cDot == c = False
            | otherwise = True 

readPartNumbers :: [String] -> [Int]
readPartNumbers = concat . map readRowPartNumbers 

readRowPartNumbers :: String -> [Int]
readRowPartNumbers [] = []
readRowPartNumbers xs = [partNumber] ++ readRowPartNumbers remainingPartNumbers
    where
        (partNumber,remainingPartNumbers) = extractInt xs

findParts :: [String] -> [String]
findParts []        = []
findParts schematic = expandedSchematic
                        [ foundParts | 
                            row <- [1..rows],
                            let prevRow     = schematic !! (row - 1),
                            let currentRow  = schematic !! row,
                            let nextRow     = schematic !! (row + 1),
                            let foundParts  = findParts' [prevRow,currentRow,nextRow] columns ]
    where
        rows    = length schematic - 2
        columns = length (head schematic) - 2

findParts' :: [String] -> Int -> String
findParts' schematic3Rows columns = 
    [ checkedElement |  column <- [1..columns],
                        let checkedElement = checkElement schematic3Rows column ] 
        
checkElement :: [String] -> Int -> Char
checkElement schematic3Rows column
    | isNumber && not isPartNumber  = cDot
    | otherwise                     = element
        where
            element         = schematic3Rows !! 1 !! column
            isNumber        = isDigit element
            isPartNumber    = checkForPartNumber schematic3Rows column

checkForPartNumber :: [String] -> Int -> Bool
checkForPartNumber (prevRow:currentRow:nextRow:_) column = symbols /= []
    where 
        (startColumn,endColumn) = findIntRange currentRow column
        prevCheck       = take (endColumn - startColumn + 1) $ drop startColumn prevRow
        currentCheck    = [currentRow !! startColumn] ++ [currentRow !! endColumn]
        nextCheck       = take (endColumn - startColumn + 1) $ drop startColumn nextRow
        border          = prevCheck ++ currentCheck ++ nextCheck
        symbols         = filter isSymbol border

findIntRange :: String -> Int -> (Int,Int)
findIntRange currentRow column = 
    ( findStartRange currentRow column, findEndRange currentRow column )

findStartRange :: String -> Int -> Int
findStartRange currentRow column 
    | column == 0                       = 0
    | isDigit (currentRow !! column)    = findStartRange currentRow (column - 1)
    | otherwise                         = column 

findEndRange :: String -> Int -> Int
findEndRange currentRow column 
    | column == endPoint                = endPoint
    | isDigit (currentRow !! column)    = findEndRange currentRow (column + 1)
    | otherwise                         = column 
        where
            endPoint = length currentRow - 1

sumPartNumbers :: [String] -> Int
sumPartNumbers = sum . readPartNumbers . findParts


-- Part two

-- The (row,column)-position of all gears
gearPositions :: [String] -> [Position]
gearPositions schematic = [ (row,column) |  column <- [1..columns],
                                            row <- [1..rows],
                                            schematic !! row !! column == cGear ]
    where
        rows        = length schematic - 2
        columns     = length (head schematic) - 2

getPartNumbersNearGear :: [String] -> Position -> [Int]
getPartNumbersNearGear schematic (row,column) 
    | length partNumbers == 2   = partNumbers
    | otherwise                 = [0]
        where
            partNumbersAbove    = getPartNumberAboveOrBelowGear (schematic !! (row-1))  column
            partNumberLeft      = getPartNumberLeftOfGear       (schematic !! row)      (column-1)
            partNumberRight     = getPartNumberRightOfGear      (schematic !! row)      (column+1)
            partNumbersBelow    = getPartNumberAboveOrBelowGear (schematic !! (row+1))  column
            partNumbers         = filter (>0) 
                                      ( partNumbersAbove ++ 
                                        [partNumberLeft, partNumberRight] ++ 
                                        partNumbersBelow )

getPartNumberAboveOrBelowGear :: String -> Int -> [Int]
getPartNumberAboveOrBelowGear rowData column 
    |   isDigit (rowData !! column)     = [ getPartNumberToTheLeft rowData column]
    |   isDigit (rowData !! (column-1))   
    &&  isDigit (rowData !! (column+1)) = [ getPartNumberToTheLeft rowData (column-1),
                                            getRightPartNumber rowData (column+1) ]
    |   isDigit (rowData !! (column-1)) = [ getPartNumberToTheLeft rowData (column-1) ]
    |   isDigit (rowData !! (column+1)) = [ getRightPartNumber rowData (column+1) ]
    |   otherwise                       = [0] 

getPartNumberToTheLeft :: String -> Int -> Int
getPartNumberToTheLeft rowData column   
    | column == 0                   = gear
    | isDigit (rowData !! column)   = getPartNumberToTheLeft rowData (column-1)
    | otherwise                     = gear
        where 
            (gear,_) = extractInt (drop column rowData)

getPartNumberLeftOfGear :: String -> Int -> Int
getPartNumberLeftOfGear rowData column  | isDigit (rowData !! column)   = getLeftPartNumber rowData column 
                                        | otherwise                     = 0

getLeftPartNumber :: String -> Int -> Int
getLeftPartNumber rowData column = read $ reverse $ takeWhile isDigit $ reverse $ take (column+1) rowData

getPartNumberRightOfGear :: String -> Int -> Int
getPartNumberRightOfGear rowData column | isDigit (rowData !! column)   = getRightPartNumber rowData column 
                                        | otherwise                     = 0

getRightPartNumber :: String -> Int -> Int
getRightPartNumber rowData column = read $ takeWhile isDigit $ drop column rowData

sumGearRatios :: [String] -> Int
sumGearRatios schematic = sum $ map product partNumbers
    where 
        partNumbers = map (getPartNumbersNearGear schematic) 
                            (gearPositions schematic)


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 3  (Haskell)"
            schematic <- expandedSchematic <$> lines <$> readFile filename

            putStr "The sum of all of the part numbers in the engine schematic:   "
            print $ sumPartNumbers schematic

            putStr "The sum of all of the gear ratios in the engine schematic:  "
            print $ sumGearRatios schematic

            putStrLn "0K.\n"

