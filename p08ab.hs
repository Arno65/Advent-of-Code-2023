-- Advent of Code 2023 - Day 8 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of steps to reach the end (part 1):          19199
-- The number of steps to reach the end (part 2): 13663968099527

--
-- (cl) by Arno Jacobs, 2023-12-08

-- module AoC2023d08ab where

import Data.List.Split

-- Some initials
filename :: String
filename = "data/inputDay08_2023.txt"

startPoint      = "AAA" :: String
endPoint        = "ZZZ" :: String
ghostStartPoint = head startPoint
ghostEndPoint   = head endPoint
sSpace          = " "   :: String
cLeft           = 'L'   :: Char

type Node = (String,(String,String))

parseNodes :: String -> Node
parseNodes nodeInfo = (startNode,(leftNode,rightNode))
    where
        nodes       = splitOn sSpace nodeInfo
        startNode   = head nodes
        leftNode    = (tail . init) (nodes !! 2)
        rightNode   =  init (nodes !! 3)

-- Part 1

countSteps :: [String] -> String -> String -> Int
countSteps network startPoint endPoint = 
    countSteps' navigation nodes 0 endPoint startPoint
        where
            navigation  = (concat . repeat . head) network
            nodes       = map parseNodes (drop 2 network)

countSteps' :: String -> [Node] -> Int -> String -> String -> Int            
countSteps' (navigate:remainingNavigation) nodes counter endPoint startPoint
    | startPoint == endPoint    = counter
    | otherwise                 = 
        countSteps' remainingNavigation nodes (counter+1) endPoint (getNextPoint navigate nodes startPoint)

-- This node search is just a simple brute force search.
-- Start looking at the beginning of the node-list and work your way up, time and time again.
-- So it's NO binary search.
-- That binary search version was build and it worked well BUT it took five times as long!
-- 
getPoint :: [Node] -> String -> Node
getPoint []                                  _          = error "Dead link." -- there are none.
getPoint ((source,direction):remainingNodes) startPoint 
    | source == startPoint  = (source,direction)
    | otherwise             = getPoint remainingNodes startPoint 

getNextPoint :: Char -> [Node] -> String -> String
getNextPoint direction nodes startPoint 
    | direction == cLeft    = fst node
    | otherwise             = snd node 
        where
            node = snd $ getPoint nodes startPoint

-- Part 2

countGhostSteps :: [String] -> Char -> Char -> Int
countGhostSteps network ghostStartPoint ghostEndPoint = foldl lcm 1 ghostSteps
        where
            navigation  = (concat . repeat . head) network
            nodes       = map parseNodes (drop 2 network)
            startPoints = getGhostPoints ghostStartPoint nodes
            ghostSteps  = map (countGhostSteps' navigation nodes 1 ghostEndPoint) startPoints

countGhostSteps' :: String -> [Node] -> Int -> Char -> String -> Int 
countGhostSteps' (navigate:remainingNavigation) nodes counter endPoint startPoint 
    | allGhostEndPoints = counter
    | otherwise         = countGhostSteps' remainingNavigation nodes (counter+1) endPoint nextStartPoint
        where
            nextStartPoint      = getNextPoint navigate nodes startPoint
            allGhostEndPoints   = (nextStartPoint !! 2) == endPoint

getGhostPoints :: Char -> [Node] -> [String]
getGhostPoints ghost = map fst . filter (\(node,_) -> node !! 2 == ghost)


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 8  (Haskell)"
            network <- lines <$> readFile filename
            putStr   "The number of steps to reach the end (part 1):          "
            print $ countSteps network startPoint endPoint 
            putStr   "The number of steps to reach the end (part 2): "
            print $ countGhostSteps network ghostStartPoint ghostEndPoint             
            putStrLn "0K.\n"


