import System.IO
import Data.Char (isSpace)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let reports = map (map read . words) (lines (trim contents))
      part1Result = length (filter isSafe reports)
      part2Result = length (filter (isSafeWithDampener isSafe) reports)
  putStrLn $ "Part 1: " ++ show part1Result
  putStrLn $ "Part 2: " ++ show part2Result

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

isSafe :: [Int] -> Bool
isSafe levels =
  (isIncreasing levels || isDecreasing levels) && diffsOK levels

isIncreasing :: [Int] -> Bool
isIncreasing xs = all (\(a,b) -> b > a) (adjPairs xs)

isDecreasing :: [Int] -> Bool
isDecreasing xs = all (\(a,b) -> b < a) (adjPairs xs)

adjPairs :: [a] -> [(a,a)]
adjPairs (x:y:rest) = (x,y):adjPairs(y:rest)
adjPairs _ = []

diffsOK :: [Int] -> Bool
diffsOK xs = all (\(a,b) -> let d = abs (b-a) in d >= 1 && d <= 3) (adjPairs xs)

isSafeWithDampener :: ([Int] -> Bool) -> [Int] -> Bool
isSafeWithDampener checker xs =
  checker xs || any (checker . removeIndex xs) [0..length xs - 1]

removeIndex :: [a] -> Int -> [a]
removeIndex xs i = take i xs ++ drop (i+1) xs
