-- Advent of Code 2023 - Day 16 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of tiles that end up being energized is:          8323
-- The maximum number of tiles that end up being energized is:  8491
--
-- Part 2 needs almost 4 minutes to complete (on my system)

-- (cl) by Arno Jacobs, 2023-12-16

-- module AoC2023d16ab where

import Data.List (nub)

type Location   = (Int,Int)
type Direction  = (Int,Int)
type Light      = (Location,Direction)
type Beam       = [Light]

-- We start with one beam at location (0,0), direction (1,0)
startBeam :: Beam
startBeam = [((0,0),(1,0))]


-- Some initials
filename :: String
filename = "data/inputDay16_2023.txt"


getMaxSize :: [[a]] -> Location
getMaxSize grid | maxY == 0 = (0,0)
                | otherwise = (maxX,maxY)
    where
        maxY = length grid
        maxX = length $ head grid

energized :: Beam -> Int
energized = length . nub . map fst


-- Part 1

workCavern :: [String] -> Beam -> Int
workCavern cavern startLight = energized path
    where
        maxXY   = getMaxSize cavern
        path    = lightBeam cavern maxXY startLight startLight

lightBeam :: [String] -> Location -> Beam -> Beam -> Beam
lightBeam cavern maxXY history currentLights
    | newLights == []   = history
    | otherwise         = lightBeam cavern maxXY newHistory newLights
        where
            newLights  = concat [ moveLightBeam cavern maxXY history cl | cl <- currentLights ]
            newHistory = history ++ newLights

-- This can result in 0, 1 or 2 light-beams
moveLightBeam :: [String] -> Location -> Beam -> Light -> Beam
moveLightBeam cavern (maxX,maxY) history ((lx,ly),(dx,dy)) = newLights
        where
            newLights   = filter (notInHistory history)
                            [ ((nx,ny),(dx',dy')) |   
                                (dx',dy') <- getDirections cavern (lx,ly) (dx,dy),
                                let nx = lx + dx',  nx >= 0 && nx < maxX,
                                let ny = ly + dy',  ny >= 0 && ny < maxY ]
        --  notInHistory :: Beam -> Light -> Bool
            notInHistory path step = not $ elem step path

getDirections :: [String] -> Location -> Direction -> [Direction]
getDirections cavern (lx,ly) (dx,dy)
    | dc == '.'     = [(dx,dy)]     -- empty space, move along
    | dc == '\\'    = [(dy,dx)]  
    | dc == '/'     = [(-dy,-dx)]
    | dc == '-'     = if dy == 0 then [(dx,dy)] else [(-1,0),(1,0)]
    | dc == '|'     = if dx == 0 then [(dx,dy)] else [(0,-1),(0,1)]
    | otherwise     = error "Illegal cavern direction!"
        where
            dc = cavern !! ly !! lx


-- Part 2

maxEnergized :: [String] -> Int
maxEnergized cavern = maximum $ energizedTlTr ++ energizedBlBr ++ energizedTlBl ++ energizedTrBr
    where
        (mx,my) = getMaxSize cavern
        -- Top left to top right, going down
        energizedTlTr = [ workCavern cavern [((x,0),   ( 0, 1))] | x <- [0..mx-1] ] 
        -- Bottom left to bottom right, going up
        energizedBlBr = [ workCavern cavern [((x,my-1),( 0,-1))] | x <- [0..mx-1] ] 
        -- Top left to bottom left, going right
        energizedTlBl = [ workCavern cavern [((0,y),   ( 1, 0))] | y <- [0..my-1] ] 
        -- Top right to bottom right, going down
        energizedTrBr = [ workCavern cavern [((mx-1,y),(-1, 0))] | y <- [0..my-1] ] 


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 16 (Haskell)"
            cavern <- lines <$> readFile filename            
            putStr "The number of tiles that end up being energized is:         "
            print $ workCavern cavern startBeam
            putStr "The maximum number of tiles that end up being energized is: "
            print $ maxEnergized cavern
            putStrLn "0K.\n"

