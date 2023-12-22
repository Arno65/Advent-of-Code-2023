///     Advent of Code 2023 - Day 21 part One and Two
///     Solutions in Haskell and Swift
///     (Ter leering ende vermaeck...)
///
///     The number of garden plots the Elf could reach in exactly       64 steps:            3532
///     The number of garden plots the Elf could reach in exactly 26501365 steps: 590104708070703
/// 
/// 
///     (cl) by Arno Jacobs, 2023-12-21

import Cocoa
 
let filename = "Code/Advent/AoC_2023/data/inputDay21_2023.txt"

let stepsPart1  = 64
let stepsPart2  = 26501365

var start1XY    = 0     ///  square grid
var start2XY    = 0     ///  also square for expanded grid

var polySteps   = 0
var remainder   = 0

/// Global 'var' grids for maximum performance 
//// grid [y][x]
var grid:       [[Character]]   = []
var startGrid:  [[Character]]   = []
var nextGrid:   [[Character]]   = []

let cS: Character   = "S"
let cDot: Character = "."

// The read lines function
func readLines () {
    if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first
    {
        let fileURL = dir.appendingPathComponent(filename)
        do {
            let readGrid = try String(contentsOf: fileURL, encoding: .utf8)            
            let lines = readGrid.split(separator: "\n")

            var helpLine: [Character] = []
            for line in lines {
                helpLine.removeAll()
                for c in line {
                    helpLine.append(c)
                }
                grid.append(helpLine)
            }
        }
        catch { print ("Read error?") }
    }
    cleanNextGrid()
    nextToStartGrid()
    start1XY = grid.count / 2 
    grid [start1XY][start1XY] = cDot
}

func nextToStartGrid () {
    var helpLine: [Character] = []
    startGrid.removeAll()
    for nextLine in nextGrid {
        helpLine.removeAll()
        for e in nextLine {
            helpLine.append(e)
        }
        startGrid.append(helpLine)
    }
}

func cleanNextGrid () {
    var helpLine: [Character] = []
    nextGrid.removeAll()
    for line in grid {
        helpLine.removeAll()
        for e in line {
            helpLine.append(e)
        }
        nextGrid.append(helpLine)
    }
}

/// The data set is read in 'grid'
//// With the next function the 'grid' is expanded 5 times in each direction
func expandGrid5X5 () {
    var helpLine: [Character] = []
    // Use the help of 'nextGrid' for the - copy 5X5 to 'grid'
    // clear start point
    grid [start1XY][start1XY] = cDot

//  row - row - row - row - row 
    nextGrid.removeAll()
    for line in grid {
        helpLine.removeAll()
        for _ in 1...5 {
            for e in line {
                helpLine.append(e)
            }
        }
        nextGrid.append(helpLine)
    }

//  block - block - block - block - block 
    grid.removeAll ()
    for _ in 1...5 {
        for line in nextGrid {
            helpLine.removeAll()
            for e in line {
                helpLine.append(e)
            }
            grid.append(helpLine)
        }
    }

    // re-enter start point
    start2XY = grid.count / 2
    grid [start2XY][start2XY] = cS
    cleanNextGrid()
    nextToStartGrid()
    // Clean 'grid'
    grid [start2XY][start2XY] = cDot
}

//// Walk the Walk
///  Work one step and count the number of possible locations
func oneStep () -> Int {
    cleanNextGrid ()
    var locations = 0
    let mx = grid[1].count - 1
    let my = grid.count - 1
    for y in 1...my-1 {
        let yp = y + 1
        let ym = y - 1
        for x in 1...mx-1 {
            if (startGrid[y][x] == cS) {
                let xp = x + 1
                let xm = x - 1
                if (nextGrid[yp][x] == cDot) { nextGrid [yp][x] = cS; locations += 1 }
                if (nextGrid[ym][x] == cDot) { nextGrid [ym][x] = cS; locations += 1 }
                if (nextGrid[y][xp] == cDot) { nextGrid [y][xp] = cS; locations += 1 }
                if (nextGrid[y][xm] == cDot) { nextGrid [y][xm] = cS; locations += 1 }
            }
        }
    }
    nextToStartGrid ()
    return locations
}

func multipleSteps ( steps: Int ) -> Int {
    var locations = 0
    for _ in 1...steps {
        locations = oneStep()
    }
    return locations
}

func solvePolynomial (  first: Int, second: Int,  third: Int, 
                        steps: Int, pattern: Int, delta: Int ) -> UInt64 {

    let x           = UInt64(steps -   delta)
    let linear      = UInt64(second -  first)
    let quadratic   = UInt64(third -   first)
    let a_num       = UInt64((quadratic - 2 * linear) / 2)
    let a_den       = UInt64(pattern * pattern)
    let b_num       = UInt64(linear - a_num)
    let b_den       = UInt64(pattern)
    let c_num       = UInt64(first)
    let c_den       = UInt64(1)

    return  (( a_num * x * x )  / a_den ) + 
            (( b_num *     x )  / b_den ) + 
               c_num            / c_den
}

//  ------------- Main program -------------
//
func runAOCmain () {
    print ("Advent of Code 2023 - day 21 - both parts in Swift")
    readLines()

    /// Part 1
    let locations = multipleSteps(steps: stepsPart1)
    print ("The number of garden plots the Elf could reach in exactly")
    print ("      \(stepsPart1) steps is:            \(locations)")

    /// Part 2
    polySteps = grid.count
    remainder = stepsPart2 % polySteps
    expandGrid5X5()

    let locationsC          = multipleSteps(steps: remainder)
    let locationsLinear     = multipleSteps(steps: polySteps)
    let locationsQuadratic  = multipleSteps(steps: polySteps)

    let bigLocations = solvePolynomial (first:      locationsC, 
                                        second:     locationsLinear, 
                                        third:      locationsQuadratic,
                                        steps:      stepsPart2,  
                                        pattern:    polySteps,
                                        delta:      remainder )

    print ("\(stepsPart2) steps is: \(bigLocations)")

    print ("0K.\n")
}

runAOCmain ()


