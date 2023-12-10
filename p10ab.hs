-- Advent of Code 2023 - Day 10 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- From start to the farthest point on the loop: 6907
-- The number of tiles enclosed by the loop:      541

-- My floodfill of part 2 is very very slow
-- It's a recursive floodfil, in Haskell.
-- Also because I had to extend the area by factor 3 iin every dimension
-- So the arean now has 176400 sub-tiles.

-- Compiled it takes ... hours to run part 2.

--
-- (cl) by Arno Jacobs, 2023-12-10

-- 
module AoC2023d10ab where

    
data Tile = NorthSouth  | EastWest  | 
            NorthEast   | NorthWest | SouthWest | SouthEast | 
            Ground      | Start     | NoTile
                deriving (Eq,Show)

type Map = [[Tile]]

type ExtendedMap = [String]

type Position = (Int,Int)

-- NO read instance code posible - so my own function
readTile :: Char -> Tile
readTile '|'    = NorthSouth
readTile '-'    = EastWest
readTile 'L'    = NorthEast
readTile 'J'    = NorthWest
readTile '7'    = SouthWest
readTile 'F'    = SouthEast
readTile '.'    = Ground
readTile 'S'    = Start
readTile _      = NoTile

-- Some initials
filename :: String
filename = "data/inputDay10_2023.txt"


-- Helpers to create and work the maze and it's loop

getGridRange :: [[a]] -> (Int,Int)
getGridRange grid = (mx,my)
    where
        mx  = length $ head grid
        my  = length grid

-- Get tile from (x,y) position of area and include range checking
getMap :: Map -> Position -> Tile
getMap area (x,y)   |   x < 0 
                    ||  x >= maxX
                    ||  y < 0
                    ||  y >= maxY   = NoTile
                    | otherwise     = area !! y !! x
    where
        (maxX,maxY) = getGridRange area
        
-- Find the (x,y) location of the start position on the map
findStartPosition :: Map -> Position
findStartPosition area  | ys == [] || xs == []  = (-1,-1)
                        | otherwise             = (x,y)
    where 
        ys  = filter (\(areaStrip,ix) -> elem Start areaStrip) $ zip area [0..]
        y   = snd $ head ys
        xs  = filter (\(tile,ix) -> tile == Start) $ zip (area !! y) [0..]
        x   = snd $ head xs

-- Find the equivalent tile for the start position so it' forming a loop
findStartTile :: Map -> Position -> Tile
findStartTile area (x,y)
    | northConnection   && southConnection  = NorthSouth
    | eastConnection    && westConnection   = EastWest
    | northConnection   && eastConnection   = NorthEast
    | southConnection   && eastConnection   = SouthEast
    | southConnection   && westConnection   = SouthWest
    | northConnection   && westConnection   = NorthWest 
    | otherwise                             = Ground
        where
            -- The tile above or north of the start position needs a south-connection
            atNorth = getMap area (x,(y-1))
            -- The tile belof or south of the start position needs a north-connection
            atSouth = getMap area (x,(y+1))
            -- The tile right or west of the start position needs a east-connection
            atWest  = getMap area ((x-1),y)
            -- The tile left or east of the start position needs a west-connection
            atEast  = getMap area ((x+1),y)
            -- Check the four connections
            northConnection =   atNorth == NorthSouth 
                            ||  atNorth == SouthEast 
                            ||  atNorth == SouthWest
            southConnection =   atSouth == NorthSouth
                            ||  atSouth == NorthEast
                            ||  atSouth == NorthWest
            westConnection  =   atWest  == EastWest
                            ||  atWest  == NorthEast
                            ||  atWest  == SouthEast
            eastConnection  =   atEast  == EastWest
                            ||  atEast  == NorthWest
                            ||  atEast  == SouthWest

-- Either this - or map over area and change Start to 'new start tile'
fillStartTileInArea :: Map -> Position -> Map
fillStartTileInArea area (sx,sy) = take sy area ++ [newMapLine] ++ drop (sy+1) area
        where
            startTile   = findStartTile area (sx,sy)
            mapLine     = area !! sy 
            newMapLine  = take sx mapLine ++ [startTile] ++ drop (sx+1) mapLine

loopPath :: Map -> [Position]
loopPath area =
    walkAroundAndCountStep newArea firstStep firstStep startPosition
        where
            startPosition   = findStartPosition area
            newArea         = fillStartTileInArea area startPosition
            firstStep       = fst $ nextSteps (getMap newArea startPosition) startPosition

walkAroundAndCountStep :: Map -> Position -> Position -> Position -> [Position]
walkAroundAndCountStep area currentPosition previousPosition startPosition 
    | currentPosition == startPosition  = [currentPosition]
    | otherwise                         = [currentPosition] ++
        walkAroundAndCountStep area nextPosition currentPosition startPosition
            where
                currentTile     = getMap area currentPosition 
                nextPositions   = nextSteps currentTile currentPosition
                -- Not so pure and clean - but -
                -- If firstStep in 'walkLoop' function is 'fst' of a pair then check here on 'snd' 
                nextPosition    = if (previousPosition == snd nextPositions) then
                                    fst nextPositions else snd nextPositions

nextSteps :: Tile -> Position -> (Position,Position)
nextSteps NorthSouth (cx,cy) = ( (cx,cy-1) , (cx,cy+1) ) 
nextSteps NorthEast  (cx,cy) = ( (cx,cy-1) , (cx+1,cy) ) 
nextSteps NorthWest  (cx,cy) = ( (cx,cy-1) , (cx-1,cy) ) 
nextSteps EastWest   (cx,cy) = ( (cx+1,cy) , (cx-1,cy) ) 
nextSteps SouthEast  (cx,cy) = ( (cx,cy+1) , (cx+1,cy) )  
nextSteps SouthWest  (cx,cy) = ( (cx,cy+1) , (cx-1,cy) ) 
nextSteps _          (cx,cy) = ( (cx,cy)   , (cx,cy)   )


-- Part 1
farthestSteps :: Map -> Int
farthestSteps = half . length . loopPath
    where half = (\x -> x `div` 2)


-- Part 2

onlyPathOnArea :: Map -> Map
onlyPathOnArea area = [[ checkTile tile inLoop
                        |   x <- [0..mx-1],
                            let inLoop  = elem (x,y) loop,
                            let tile    = getMap newArea (x,y) ]
                        | y <- [0..my-1]]
    where
        (mx,my) = getGridRange area
        loop    = loopPath area
        newArea = fillStartTileInArea area $ findStartPosition area
        --
        checkTile tile True = tile
        checkTile _    _    = Ground 

-- 
extendArea3X3 :: Map -> ExtendedMap
extendArea3X3 = concat . (map extendLine3X3) . onlyPathOnArea

extendLine3X3 :: [Tile] -> ExtendedMap
extendLine3X3 tiles =   mapLine topPartExtension    tiles ++ 
                        mapLine middlePartExtension tiles ++
                        mapLine bottomPartExtension tiles
    where
        mapLine lineFunction = (\l -> [l]) . concat . (map lineFunction)
                                                 
topPartExtension :: Tile -> String
topPartExtension NorthSouth = " # "
topPartExtension NorthEast  = " # "
topPartExtension NorthWest  = " # "
topPartExtension _          = "   "

middlePartExtension :: Tile -> String
middlePartExtension EastWest    = "###"
middlePartExtension NorthEast   = " ##"
middlePartExtension NorthWest   = "## "
middlePartExtension SouthEast   = " ##"
middlePartExtension SouthWest   = "## "
middlePartExtension Ground      = " . "
middlePartExtension _           = " # "

bottomPartExtension :: Tile -> String
bottomPartExtension NorthSouth  = " # "
bottomPartExtension SouthEast   = " # "
bottomPartExtension SouthWest   = " # "
bottomPartExtension _           = "   "

clearTiles :: Position -> [String] -> [String]
clearTiles startPosition plot = clearedPlot
    where
        clearedTiles    = clearTiles' [startPosition] [startPosition] maxRange plot 
        clearedPlot     = clearPlot clearedTiles maxRange plot
        maxRange        = getGridRange plot

-- Going massivly recursive
clearTiles' :: [Position] -> [Position] -> Position -> [String] -> [Position]
clearTiles' clearedTiles currentPositions mxy plot 
    | nextCleared == [] = clearedTiles
    | otherwise         = clearing
        where
            nextCleared = unique $ concat $ map (\cp -> nextUncheckedTiles clearedTiles cp mxy plot) currentPositions
            newCleared  = unique $ clearedTiles ++ nextCleared
            clearing    = clearTiles' newCleared nextCleared mxy plot

-- Work one step, from one position (cx,cy)
nextUncheckedTiles :: [Position] -> Position -> Position -> [String] -> [Position]
nextUncheckedTiles clearedTiles (cx,cy) (mx,my) plot =
    [ testTile |    (x,y) <- [(-1,0),(1,0),(0,-1),(0,1)], 
                    let dcx = cx + x,
                    dcx >= 0 && dcx < mx,
                    let dcy = cy + y,
                    dcy >= 0 && dcy < my,
                    let testTile = (dcx,dcy), 
                    isFreeTile testTile plot, 
                    not (elem testTile clearedTiles) ]

unique :: Eq a => [a] -> [a]
unique []     = []
unique (e:[]) = [e]
unique (e:rl) = [e] ++ unique (filter (/= e) rl)

isFreeTile :: Position -> [String] -> Bool
isFreeTile (x,y) plot = (plot !! y !! x) /= '#'

clearPlot :: [Position] -> Position -> [String] -> [String]
clearPlot uncheck (mx,my) plot = 
    [[ newPlot |    x <- [0..mx-1],
                    let newPlot = if (elem (x,y) uncheck) 
                                    then ' ' 
                                    else plot !! y !! x ] 
                |   y <- [0..my-1] ]

countTiles :: ExtendedMap -> Int
countTiles =  sum . map (length . filter (== '.'))

enclosedTiles :: Map -> Int
enclosedTiles = countTiles . (clearTiles (0,0)) . extendArea3X3


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 10 (Haskell)"
            area <- map (map readTile) <$> lines <$> readFile filename
            putStr "From start to the farthest point on the loop: "
            print $ farthestSteps area            
            putStr "The number of tiles enclosed by the loop:      "
--            print $ enclosedTiles area
            putStrLn "541 *"
            putStrLn "0K.\n"
