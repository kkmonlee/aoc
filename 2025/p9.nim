# nim c -d:release -d:danger --opt:speed -o:p9 p9.nim && ./p9
import std/[strutils, tables]

proc main() =
    var points: seq[(int64, int64)]
    
    for line in lines("input/input_9.txt"):
        let parts = line.split(',')
        points.add((parseBiggestInt(parts[0]), parseBiggestInt(parts[1])))
    
    let n = points.len
    
    var maxAreaP1: int64 = 0
    for i in 0 ..< n:
        for j in i + 1 ..< n:
            let width = abs(points[j][0] - points[i][0]) + 1
            let height = abs(points[j][1] - points[i][1]) + 1
            maxAreaP1 = max(maxAreaP1, width * height)
    
    echo "Part 1: ", maxAreaP1
    
    var validRanges = initTable[int64, (int64, int64)]()
    var globalMinY = int64.high
    var globalMaxY = int64.low
    
    for i in 0 ..< n:
        let (x1, y1) = points[i]
        let (x2, y2) = points[(i + 1) mod n]
        
        if x1 == x2:
            let ymin = min(y1, y2)
            let ymax = max(y1, y2)
            globalMinY = min(globalMinY, ymin)
            globalMaxY = max(globalMaxY, ymax)
            for y in ymin .. ymax:
                if y in validRanges:
                    validRanges[y] = (min(validRanges[y][0], x1), max(validRanges[y][1], x1))
                else:
                    validRanges[y] = (x1, x1)
        else:
            let xmin = min(x1, x2)
            let xmax = max(x1, x2)
            globalMinY = min(globalMinY, y1)
            globalMaxY = max(globalMaxY, y1)
            if y1 in validRanges:
                validRanges[y1] = (min(validRanges[y1][0], xmin), max(validRanges[y1][1], xmax))
            else:
                validRanges[y1] = (xmin, xmax)
    
    var maxAreaP2: int64 = 0
    
    for i in 0 ..< n:
        let (x1, y1) = points[i]
        for j in i + 1 ..< n:
            let (x2, y2) = points[j]
            
            let minRx = min(x1, x2)
            let maxRx = max(x1, x2)
            let minRy = min(y1, y2)
            let maxRy = max(y1, y2)
            
            if minRy < globalMinY or maxRy > globalMaxY:
                continue
            
            let potentialArea = (maxRx - minRx + 1) * (maxRy - minRy + 1)
            if potentialArea <= maxAreaP2:
                continue
            
            var allValid = true
            for y in minRy .. maxRy:
                if y notin validRanges:
                    allValid = false
                    break
                let (lo, hi) = validRanges[y]
                if minRx < lo or maxRx > hi:
                    allValid = false
                    break
            
            if allValid:
                maxAreaP2 = potentialArea
    
    echo "Part 2: ", maxAreaP2

main()