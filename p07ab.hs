-- Advent of Code 2023 - Day 7 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The total winnings (part 1): 248812215
-- The total winnings (part 2): 250057090
--
-- (cl) by Arno Jacobs, 2023-12-07

-- 
module AoC2023d07ab where

import Data.List
import Data.List.Split


data Hand   =   FiveOfaKind 
            |   FourOfaKind
            |   FullHouse
            |   ThreeOfaKind
            |   TwoPair
            |   OnePair
            |   HighCard
                deriving (Eq,Ord,Show)

data CardPack1  = CP1_Ace | CP1_King | CP1_Queen | CP1_Jack 
                | CP1_10 | CP1_9 | CP1_8 | CP1_7 | CP1_6 | CP1_5 | CP1_4 | CP1_3 | CP1_2 
                | CP1_IllegalCard
                    deriving (Eq,Ord,Show)

type CardsPack1 = [CardPack1]

data CardPack2  = CP2_Ace | CP2_King | CP2_Queen 
                | CP2_10 | CP2_9 | CP2_8 | CP2_7 | CP2_6 | CP2_5 | CP2_4 | CP2_3 | CP2_2 
                | CP2_Joker
                | CP2_IllegalCard
                    deriving (Eq,Ord,Show)

type CardsPack2 = [CardPack2]

-- Custom reader for both card packs
--
readCardPack1 :: Char -> CardPack1
readCardPack1 '2'   = CP1_2
readCardPack1 '3'   = CP1_3
readCardPack1 '4'   = CP1_4
readCardPack1 '5'   = CP1_5
readCardPack1 '6'   = CP1_6
readCardPack1 '7'   = CP1_7
readCardPack1 '8'   = CP1_8
readCardPack1 '9'   = CP1_9
readCardPack1 'T'   = CP1_10
readCardPack1 'J'   = CP1_Jack
readCardPack1 'Q'   = CP1_Queen
readCardPack1 'K'   = CP1_King
readCardPack1 'A'   = CP1_Ace
readCardPack1 _     = CP1_IllegalCard

readCardPack2 :: Char -> CardPack2
readCardPack2 'J'   = CP2_Joker
readCardPack2 '2'   = CP2_2
readCardPack2 '3'   = CP2_3
readCardPack2 '4'   = CP2_4
readCardPack2 '5'   = CP2_5
readCardPack2 '6'   = CP2_6
readCardPack2 '7'   = CP2_7
readCardPack2 '8'   = CP2_8
readCardPack2 '9'   = CP2_9
readCardPack2 'T'   = CP2_10
readCardPack2 'Q'   = CP2_Queen
readCardPack2 'K'   = CP2_King
readCardPack2 'A'   = CP2_Ace
readCardPack2 _     = CP2_IllegalCard

-- Some initials
filename :: String
filename = "data/inputDay07_2023.txt"

sSpace = " " :: String
cJoker = 'J' :: Char


getIndex :: Eq a => [a] -> a -> Int
getIndex testList testElement   | matches == [] = -1
                                | otherwise     = snd $ head matches
    where
        matches = filter (\(element,_) -> element == testElement) $ zip testList [0..]

countSets :: Ord a => [a] -> [Int]
countSets = sort . countSets' . sort
    where
        countSets' [] = []
        countSets' xs = [setLength] ++ countSets' restSet
            where
                (setLength,restSet) = countSet 0 xs 
                --
                countSet setLength []       = ( 0, [] ) 
                countSet setLength (x:xs)   = ( setLength, remainingSet )
                    where                                
                        sameElements    = takeWhile (== x) xs
                        remainingSet    = dropWhile (== x) xs
                        setLength       = 1 + length sameElements

getHand :: [Int] -> Hand
getHand hand    | hand == [5]       = FiveOfaKind
                | hand == [1,4]     = FourOfaKind
                | hand == [2,3]     = FullHouse
                | hand == [1,1,3]   = ThreeOfaKind
                | hand == [1,2,2]   = TwoPair
                | hand == [1,1,1,2] = OnePair
                | otherwise         = HighCard 

-- Quick REVERSE sort for hand strengths
sortHands :: Ord a => [( Hand, ( [a], Int ))] -> [( Hand, ( [a], Int ))]
sortHands []        = []
sortHands (h:hs)    = sortHands bigger ++ [h] ++ sortHands smaller
    where
        smaller = filter (<  h) hs
        bigger  = filter (>= h) hs

-- Part 1

parseHandPart1 :: String -> ( Hand, ( CardsPack1, Int ))
parseHandPart1 cardsSet = ( getHand hand, (map readCardPack1 cards, read score ))
    where
        (cards:score:_) = splitOn sSpace cardsSet
        hand            = countSets cards


-- Part 2

parseHandPart2 :: String -> ( Hand, ( CardsPack2, Int ))
parseHandPart2 cardsSet = ( getHand bestHand, (map readCardPack2 cards, read score ))
    where
        (cards:score:_) = splitOn sSpace cardsSet
        bestHand        = (countSets . getBestHand) cards

-- Change the Joker for the card that gives the best hand.
getBestHand :: String -> String
getBestHand cards   | cards == jokers   = "22222"   -- or "AAAAA", it doesn't matter
                    | jokers == []      = cards
                    | otherwise         = bestHand
    where
        jokers      = filter (\card -> card == cJoker) cards 
        rest        = filter (\card -> card /= cJoker) cards 
        bestCard    = (snd . head . sortTallyCards . tallyList) rest
        bestHand    = [ newCard |   card <- cards, 
                                    let newCard = if card == cJoker then bestCard else card ]

-- With a tally select the most numerous card in hand, except for the Joker...
--
tallyList :: Eq a => [a] -> [(Int,a)]
tallyList []        = []
tallyList (e:es)    = [(count,e)] ++ tallyList (filter (/=e) es)
    where
        count   = 1 + length (filter (==e) es)

sortTallyCards :: Ord a =>  [(Int,a)] ->  [(Int,a)]
sortTallyCards []           = []
sortTallyCards (tally:rt)   = sortTallyCards bigger ++ [tally] ++ sortTallyCards smaller
    where
        smaller = filter (< tally) rt
        bigger  = filter (> tally) rt

-- Main code 

totalWinnings :: [ ( Hand, ( [a], Int ))] -> Int -> Int
totalWinnings []                    _      = 0
totalWinnings (hand:remainingHands) factor =
    strength + totalWinnings remainingHands (factor + 1)
        where
            strength = factor * (snd . snd) hand

main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 7  (Haskell)"
            cards <- lines <$> readFile filename

            putStr   "The total winnings (part 1): "
            let part1 = sortHands $ map parseHandPart1 cards
            print $ totalWinnings part1 1

            putStr   "The total winnings (part 2): "
            let part2 = sortHands $ map parseHandPart2 cards
            print $ totalWinnings part2 1
            putStrLn "0K.\n"


