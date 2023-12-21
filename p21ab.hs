-- Advent of Code 2023 - Day 21 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of garden plots the Elf could reach in exactly       64 steps:            3532
-- The number of garden plots the Elf could reach in exactly 26501365 steps: 590104708070703
-- 
--
-- (cl) by Arno Jacobs, 2023-12-21

-- Part 1 still takes ~7 seconds for the compiled version
-- Part 2 takes 3+ hours...  :-(
-- The first 65 is done in ~7 seconds but then next 131 take up over 20 minutes!
-- But it will get there in the end. 
--
-- Observations for part 2
-- The starting point is in the centre of the garden.
-- The column and row of the starting point are BOTH blank
-- ( I immediately saw that the starting row had no rocks. 
--   Then I took a better look at the start column. )
--
-- The single garden plot has a range of 131 X 131
-- 26501365 modulo 131  is  65
-- There is a quadratic pattern after each 131 steps
-- 65 + 131 + 131 = 327 and is the maximum number of steps taken
-- The expansion of the grid by a factor 5 both ways is enough
--
-- step:  65 	positions:  3703
-- step: 196 	positions: 32712
-- step: 327 	positions: 90559
--
-- Solve - Gaussian elimination
--
-- polynomial:  14419 / (131^2) * x^2 + 
--              14590 / (131^1) * x +
--               3703 / (131^0) 
-- And x = steps - 65
-- Part 2 for 26501365 steps, so calculate for x = 26501300 -->  590104708070703

module AoC2023d21ab where


type Garden     = [String]
type Location   = (Int,Int)
type Locations  = [Location]

-- Some initials

filename :: String
filename = "data/inputDay21_2023.txt"

stepsPart1,stepsPart2 :: Int
stepsPart1 =       64
stepsPart2 = 26501365

cStart      = 'S' :: Char
cGardenPlot = '.' :: Char
cRock       = '#' :: Char

directions :: [Location]
directions = [(-1,0),(1,0),(0,-1),(0,1)]

unique :: Eq a => [a] -> [a]
unique []       = []
unique (e:es)   = [e] ++ unique (filter (/= e) es)

getMaximumRange :: [[a]]-> Location
getMaximumRange garden = (maxX,maxY)
    where
        maxY = length garden
        maxX = length $ head garden

addPair :: Num a => (a,a) -> (a,a) -> (a,a)
addPair (x1,y1) (x2,y2) = (x1+x2,y1+y2)

inRange ::  (Num a,Ord a) => (a,a) -> (a,a) -> Bool
inRange (maxX,maxY) (x,y)   =   x >= 0 && x < maxX
                            &&  y >= 0 && y < maxY

isNotElement :: Eq a => a -> [[a]] -> Location -> Bool
isNotElement element grid (tx,ty) = grid !! ty !! tx /= element 

oneStep :: Location -> Garden -> Location -> Locations
oneStep currentLocation garden range =
    [ nextXY |  dXY <- directions, 
                let nextXY = addPair currentLocation dXY,
                inRange range nextXY,
                isNotElement cRock garden nextXY ]

allOneSteps :: Locations -> Garden -> Location -> Locations
allOneSteps currentLocations garden range = unique allNextSteps
    where
        allNextSteps = concat [ oneStep start garden range | start <- currentLocations ]


getStartLocation :: Char -> Garden -> Location        
getStartLocation start garden
    | ys == [] || xs == []  = (0,0)     -- Should NOT happen but; Not found, No error... 
    | otherwise             = (head xs, y)
    where
        ys      = [ ix | (gl,ix) <- zip garden [0..], ix > minIX, elem start gl ]
        y       = head ys 
        xs      = [ ix | (gp,ix) <- zip (garden !! y) [0..], ix > minIX, start == gp ]
        minIX   = div (length garden) 2 - div (length garden) 5
        

expand :: Int -> [[a]] -> [[a]]
expand rate = map expandOneDirection . expandOneDirection
    where
        expandOneDirection = concat . (replicate rate)

workPolySteps :: Int -> [Int] -> Locations -> Garden -> Location -> [Int]
workPolySteps 0 []      currentLocations _      _     = [length currentLocations] 
workPolySteps 0 (ns:rs) currentLocations garden range =
    [length currentLocations] ++ workPolySteps ns rs currentLocations garden range
workPolySteps steps nss currentLocations garden range =
    workPolySteps (steps-1) nss nextLocations garden range
        where 
            nextLocations = allOneSteps currentLocations garden range


workInfiniteGarden :: Int -> Garden -> [Int]
workInfiniteGarden steps garden = 
    workPolySteps stepsPart1 [secondStep,polySteps,polySteps] [startLocation] gardens range
        where
            rangeOneGarden      = getMaximumRange garden
            polySteps           = fst rangeOneGarden        -- The garden is a square
            polyC               = mod steps polySteps
            secondStep          = polyC - stepsPart1
            polyStepLinear      = polySteps + polyC
            polyStepQuadratic   = polySteps + polyStepLinear
            gardens             = expand 5 garden 
            startLocation       = getStartLocation cStart gardens
            range               = getMaximumRange gardens


solvePolynomial :: [Int] -> Int -> Garden -> Integer
solvePolynomial (c:second:third:_) steps garden = locations
    where
        polySteps   = fromIntegral (fst (getMaximumRange garden))      -- The garden is a square
        constant    = mod (fromIntegral steps) polySteps  
        x           = fromIntegral steps  - constant
        linear      = fromIntegral (second - c)
        quadratic   = fromIntegral (third  - c)
        c_num       = fromIntegral c
        c_den       = 1

        -- delta steps now are [0,131,262]
        -- 131 * 131 = 17616
        -- 262 * 262 = 68644
        -- Solve:
        --   17616 * a + 131 * b = second (locations) - c = 29009
        --   68644 * a + 262 * b = third (location) - c   = 86856
        -- <=> (row 1 times 2)
        --   34322 * a + 262 * b = 58018
        --   68644 * a + 262 * b = 86856
        -- <=> (row 2 - row 1)
        --   34322 * a           = 28838
        -- => a = 28838 / 34322 = 14419 / 17161
        a_num       = div (quadratic - 2 * linear) 2
        a_den       = polySteps * polySteps
        -- and: 
        --      17616 * a + 131 * b = 29009
        -- <=> 
        --      131 * b = 29009 - 17161 * 14419 / 17161
        --            b = (29009 - 14419) / 131 = 
        b_num       = linear - a_num
        b_den       = polySteps
        locations   = div (x * x * a_num) a_den + div (x * b_num) b_den + div c_num c_den


main :: IO ()
main = do   putStrLn "Advent of Code 2023 - day 21 (Haskell)"
            garden <- lines <$> readFile filename          

            let polynomials = workInfiniteGarden stepsPart2 garden
            putStrLn "The number of garden plots the Elf could reach in exactly"
            putStr "      "
            putStr $ show stepsPart1 
            putStr " steps is:            "
            print $ head polynomials

            let locations = solvePolynomial (tail polynomials) stepsPart2 garden
            putStr $ show stepsPart2
            putStr " steps is: "
            putStr $ show locations

            putStrLn "0K.\n"

