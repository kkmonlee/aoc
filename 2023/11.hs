import qualified Data.Set as Set

type Point = (Int, Int)

parseInput :: String -> [[Char]]
parseInput = lines

findGalaxies :: [[Char]] -> [Point]
findGalaxies grid = [(r, c) | (r, row) <- zip [0..] grid, 
                             (c, ch) <- zip [0..] row, 
                             ch == '#']

getEmptyRows :: [[Char]] -> [Int]
getEmptyRows grid = [r | (r, row) <- zip [0..] grid, 
                        all (== '.') row]

getEmptyCols :: [[Char]] -> [Int]
getEmptyCols grid = [c | c <- [0..width-1], 
                        all ((== '.') . (!! c)) grid]
  where width = length (head grid)

manhattan :: Point -> Point -> Int
manhattan (r1, c1) (r2, c2) = abs (r2 - r1) + abs (c2 - c1)

countCrossings :: Int -> Int -> Int -> [Int] -> Int
countCrossings a b factor empties = 
    factor * length [x | x <- empties, min a b <= x && x <= max a b]

getTotalDistance :: Int -> [Point] -> [Int] -> [Int] -> Int
getTotalDistance factor galaxies emptyRows emptyCols = sum
    [dist + crossRows + crossCols |
     (i, g1) <- zip [0..] galaxies,
     g2 <- drop (i + 1) galaxies,
     let dist = manhattan g1 g2,
     let crossRows = countCrossings (fst g1) (fst g2) factor emptyRows,
     let crossCols = countCrossings (snd g1) (snd g2) factor emptyCols]

solve :: [[Char]] -> (Int, Int)
solve grid = (part1, part2)
  where 
    galaxies = findGalaxies grid
    emptyRows = getEmptyRows grid
    emptyCols = getEmptyCols grid
    part1 = getTotalDistance 1 galaxies emptyRows emptyCols
    part2 = getTotalDistance (1000000-1) galaxies emptyRows emptyCols

main :: IO ()
main = do
    content <- readFile "input/input_11.txt"
    let grid = parseInput content
    let (part1, part2) = solve grid
    print part1
    print part2