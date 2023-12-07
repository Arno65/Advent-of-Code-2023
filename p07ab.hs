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

-- Some initials
filename :: String
filename = "data/inputDay07_2023.txt"

sSpace = " " :: String

data Hand   =   FiveOfaKind 
            |   FourOfaKind
            |   FullHouse
            |   ThreeOfaKind
            |   TwoPair
            |   OnePair
            |   HighCard
                deriving (Eq,Ord,Show)
     
-- Card strength from HIGH 'A' to low '2'     
cardStrengthPart1 :: String
cardStrengthPart1 = "AKQJT98765432"

cardStrengthPart2 :: String
cardStrengthPart2 = "AKQT98765432J"

cJoker = 'J' :: Char


compareCardStrength :: String -> String -> String -> Int
compareCardStrength cardsStrength card1 card2 
    | cardsStrength1 <  cardsStrength2  = -1 
    | cardsStrength1 >  cardsStrength2  =  1 
    | cardsStrength1 == cardsStrength2  =  0 
        where
            cardsStrength1 = map (getIndex cardsStrength) card1 
            cardsStrength2 = map (getIndex cardsStrength) card2

getIndex :: Eq a => [a] -> a -> Int
getIndex testList testElement   | matches == [] = -1
                                | otherwise     = snd $ head matches
    where
        matches = filter (\(element,_) -> element == testElement) $ zip testList [0..]

countSets :: String -> [Int]
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
                        sameElements    = takeWhile (\c -> c == x) xs
                        remainingSet    = dropWhile (\c -> c == x) xs
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
sortHands :: String -> [(Hand,(String,Int))] -> [(Hand,(String,Int))]
sortHands _            []       = []
sortHands cardStrength (h:hs)   = sortHands cardStrength bigger ++ [h] ++ sortHands cardStrength smaller
    where
        smaller = filter (\th -> (compareHands cardStrength th h) == -1) hs
        bigger  = filter (\th -> (compareHands cardStrength th h) >=  0) hs
        -- 
        compareHands cardStrength (h1,(hs1,_)) (h2,(hs2,_))
            |   h1 < h2 
            || (h1 == h2 && (compareCardStrength cardStrength hs1 hs2 == -1)) = -1
            |   h1 > h2 
            || (h1 == h2 && (compareCardStrength cardStrength hs1 hs2 ==  1)) =  1
            | otherwise                                             =  0

-- Part 1

parseHandPart1 :: String -> (Hand,(String,Int))
parseHandPart1 cardsSet = ( getHand hand, (cards, read score ))
    where
        (cards:score:_) = splitOn sSpace cardsSet
        hand            = countSets cards

totalWinnings :: [(Hand,(String,Int))] -> Int -> Int
totalWinnings []                    _      = 0
totalWinnings (hand:remainingHands) factor =
    strength + totalWinnings remainingHands (factor + 1)
        where
            strength = factor * (snd . snd) hand

-- Part 2

parseHandPart2 :: String -> (Hand,(String,Int))
parseHandPart2 cardsSet = ( getHand bestHand, (cards, read score ))
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

sortTallyCards ::  [(Int,Char)] ->  [(Int,Char)]
sortTallyCards []           = []
sortTallyCards (tally:rt)   = sortTallyCards  bigger ++ [tally] ++ sortTallyCards smaller
    where
        smaller = filter (\tt -> (compareTally tt tally) == -1) rt
        bigger  = filter (\tt -> (compareTally tt tally) >=  0) rt
        --
        compareTally (count1,card1) (count2,card2)
            |   count1 <  count2
            || (count1 == count2 && (compareCardStrength cardStrengthPart2 [card1] [card2] == -1))  = -1
            |   count1 >  count2
            || (count1 == count2 && (compareCardStrength cardStrengthPart2 [card1] [card2] ==  1))  =  1
            | otherwise                                                                             =  0


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 7  (Haskell)"
            cards <- lines <$> readFile filename

            putStr   "The total winnings (part 1): "
            let part1 = (sortHands cardStrengthPart1) $ map parseHandPart1 cards
            print $ totalWinnings part1 1

            putStr   "The total winnings (part 2): "
            let part2 = (sortHands cardStrengthPart2) $ map parseHandPart2 cards
            print $ totalWinnings part2 1
            putStrLn "0K.\n"

