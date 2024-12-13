import Data.Array
import Data.List (nub)

type TopoMap = Array (Int, Int) Int
type Point = (Int, Int)

main :: IO ()
main = do
    input <- readFile "input/input_10.txt"
    let topoMap = parseTopoMap $ lines input
        trailheadScores = sum $ map (trailheadScore topoMap) (findTrailheads topoMap)
        trailheadRatings = sum $ map (trailheadRating topoMap) (findTrailheads topoMap)
    putStrLn $ "Part 1: " ++ show trailheadScores
    putStrLn $ "Part 2: " ++ show trailheadRatings

parseTopoMap :: [String] -> TopoMap
parseTopoMap input =
    let rows = length input
        cols = length (head input)
        elements = concatMap (map (read . (:[]))) input
    in listArray ((0, 0), (rows - 1, cols - 1)) elements

-- find all trailheads
findTrailheads :: TopoMap -> [Point]
findTrailheads topoMap = [pos | (pos, height) <- assocs topoMap, height == 0]

trailheadScore :: TopoMap -> Point -> Int
trailheadScore topoMap start = length $ dfs topoMap start 0 []

trailheadRating :: TopoMap -> Point -> Int
trailheadRating topoMap start = length $ nub $ dfsPaths topoMap start 0 []

-- count reachable "9" positions
dfs :: TopoMap -> Point -> Int -> [Point] -> [Point]
dfs topoMap (x, y) currentHeight visited
    | not (inBounds topoMap (x, y)) || topoMap ! (x, y) /= currentHeight || (x, y) `elem` visited = []
    | currentHeight == 9 = [(x, y)]
    | otherwise =
        concatMap (\(dx, dy) -> dfs topoMap (x + dx, y + dy) (currentHeight + 1) ((x, y) : visited)) directions

-- find all distinct paths
dfsPaths :: TopoMap -> Point -> Int -> [Point] -> [[Point]]
dfsPaths topoMap (x, y) currentHeight visited
    | not (inBounds topoMap (x, y)) || topoMap ! (x, y) /= currentHeight || (x, y) `elem` visited = []
    | currentHeight == 9 = [[(x, y)]]
    | otherwise =
        concatMap (\(dx, dy) -> map ((x, y) :) $ dfsPaths topoMap (x + dx, y + dy) (currentHeight + 1) ((x, y) : visited)) directions

inBounds :: TopoMap -> Point -> Bool
inBounds topoMap (x, y) =
    let ((minX, minY), (maxX, maxY)) = bounds topoMap
    in x >= minX && x <= maxX && y >= minY && y <= maxY

-- (up, down, left, right)
directions :: [(Int, Int)]
directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
