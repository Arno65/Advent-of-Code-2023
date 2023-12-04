-- Advent of Code 2023 - Day 2 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of possible game ID is:       2545
-- The sum of the power of all sets is: 78111
--
-- (cl) by Arno Jacobs, 2023-12-02

-- 
module AoC2023d02ab where

import Data.Char

-- Some initials
filename :: String
filename = "data/inputDay02_2023.txt"

type RGBvalues = (Int,Int,Int) -- rgb values


noCubes = (0,0,0) :: RGBvalues

( maxRed, maxGreen, maxBlue ) = (12,13,14) :: RGBvalues

cMinus = '-' :: Char
cSpace = ' ' :: Char

-- I didn't go for the 'splitOneOf' function
-- splitOneOf ":,;" game
-- For example resulting in: ["Game 1"," 20 green"," 3 red"," 2 blue"," 9 red",...
--

extractInt :: String -> (Int,String)
extractInt []       = (0,[])
extractInt (x:xs)   | x == cMinus   = (i,rs)
                    | isDigit x     = (i,rs)
                    | otherwise     = extractInt xs
    where
        i  = read (x:(takeWhile isDigit xs))
        rs = dropWhile isDigit xs 

isLowerCase :: Char -> Bool
isLowerCase c = (c >= 'a') && (c <= 'z')

extractColour :: String -> (String,String)
extractColour []    = ([],[])
extractColour xs    = (takeWhile isLowerCase start, dropWhile isLowerCase start)
    where
        start   = dropWhile (not . isLowerCase) xs

-- Works from start of game info
getGame :: String -> (Int,String)
getGame gameInfo = ( gameNumber, tail remainingLine)
    where
        (gameNumber,remainingLine) = extractInt gameInfo

-- part 1
-- summing all colour info
--
workGame :: String -> (Int,RGBvalues)
workGame game = (gameNumber, workColours colourInfo noCubes)
    where
        (gameNumber,colourInfo) = getGame game

-- The instructions are not clear to me
-- My interpretation is to sum all colours withing one game
-- But, the task is the maximum per set
--
-- Nice is that this part is VERY USABLE for part 2 of the challenge
--
workColours :: String -> RGBvalues -> RGBvalues
workColours []         cubesCount = cubesCount
workColours colourInfo (r,g,b)  
    | colour == "red"   = workColours remainingInfo2 ( (max r cubes), g, b )
    | colour == "green" = workColours remainingInfo2 ( r, (max g cubes), b )
    | colour == "blue"  = workColours remainingInfo2 ( r, g, (max b cubes) )
    | otherwise         = error "Could not extract colour!"
        where
            (cubes,remainingInfo1)  = extractInt colourInfo
            (colour,remainingInfo2) = extractColour remainingInfo1

possibleGame :: (Int,RGBvalues) -> Bool
possibleGame (_,(r,g,b)) = (r <= maxRed) && (g <= maxGreen) && (b <= maxBlue)

workGames :: [String] -> Int
workGames games = sumGameNumbers
    where
        countedCubes    = map workGame games
        filterGames     = filter possibleGame countedCubes
        sumGameNumbers  = sum $ map fst filterGames

-- part 2 
--
findPower :: String -> Int
findPower game = r * g * b
    where
        ( _, colourInfo )   = getGame game
        ( r, g, b )         = workColours colourInfo noCubes

findPowers :: [String] -> Int
findPowers = sum . map findPower

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 2  (Haskell)"
            games <- lines <$> readFile filename
            putStr   "The sum of possible game ID is:       "
            print $ workGames games
            putStr   "The sum of the power of all sets is: "
            print $ findPowers games
            putStrLn "0K.\n"

