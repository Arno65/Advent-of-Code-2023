-- Advent of Code 2023 - Day 4 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The total amount of points of all cards is:   23441
-- The total amount of scratchcards is:        5923918
--
-- (cl) by Arno Jacobs, 2023-12-04


-- Slow recursive algorithm.
-- RUN compiled version! That will take about a second.

-- module AoC2023d04ab where

import Data.List
import Data.List.Split

-- A helper - card tree data structure
data CardTree = CardNumber Int | Branches [CardTree]  deriving (Eq,Ord,Show)


-- Some initials
filename :: String
filename = "data/inputDay04_2023.txt"

splitLine :: String -> [String]
splitLine = splitOneOf ":|"

splitNumbers :: String -> [String]
splitNumbers = filter (/="") . splitOn " "

cardNumber :: String -> Int
cardNumber = read . last . splitNumbers . head . splitLine

winningNumbers :: String -> [Int]
winningNumbers = map read . splitNumbers . head . tail . splitLine

myNumbers :: String -> [Int]
myNumbers = map read . splitNumbers . last . splitLine

-- part 1

countMyNumbers :: [Int] -> [Int] -> Int
countMyNumbers winners = countMyNumbers' 0 winners
    where
    --  countMyNumbers' :: Int -> [Int] -> [Int] -> Int
        countMyNumbers' count []            _       = count 
        countMyNumbers' count (win:winners) mine    
            | elem win mine = countMyNumbers' (count+1) winners mine
            | otherwise     = countMyNumbers'  count    winners mine 
    
countWinningCards :: String -> Int
countWinningCards card = countMyNumbers winners mine
    where
        winners = winningNumbers card
        mine    = myNumbers card

scoreCards :: [String] -> Int
scoreCards = sum . map (power2var . countWinningCards)
    where
    --  power2var :: Int -> Int
        power2var 0 = 0 
        power2var n = 2^(n-1)


-- part 2

-- The ( card wins, card numbers ) is paired this way 
-- So it's easy to sort from low to high for the card wins
-- This will speed up the Tree algorithm by a factor 2
-- 
prepCountScratchcards :: [String] -> [(Int,Int)]
prepCountScratchcards cards = sort $ zip cardWins cardNumbers
    where
        cardNumbers = map cardNumber cards
        cardWins    = map countWinningCards cards

-- Solve the counting by creating a tree 
cardInTree :: Int -> [(Int,Int)] -> CardTree
cardInTree card cardsWinnings 
    | count == 0    = CardNumber card
    | otherwise     =
        Branches [ CardNumber card,
            Branches [ cardInTree c cardsWinnings | c <- [card+1 .. card+count]]]
            where
                count = (fst . head . filter ((== card) . snd )) cardsWinnings

countCards :: CardTree -> Int
countCards (CardNumber _)       = 1
countCards (Branches cardTree ) = sum [ countCards branch | branch <- cardTree ]

countScratchcards :: [String] -> Int
countScratchcards cards = countCards cardTree
    where
        countHelp   = prepCountScratchcards cards
        cardTree    = Branches [ cardInTree card countHelp | (_,card) <- countHelp ]

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 4  (Haskell)"
            cards <- lines <$> readFile filename
            putStr   "The total amount of points of all cards is:   "
            print $ scoreCards cards
            putStr   "The total amount of scratchcards is:        "
            print $ countScratchcards cards
            putStrLn "0K.\n"

