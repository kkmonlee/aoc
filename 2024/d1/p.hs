import System.IO
import Data.List (sort, group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
      half = length ls `div` 2
      leftList = map read (take half ls) :: [Int]
      rightList = map read (drop half ls) :: [Int]

      sortedLeft = sort leftList
      sortedRight = sort rightList
      part1 = sum $ zipWith (\a b -> abs (a - b)) sortedLeft sortedRight

      freqRight = frequencyMap rightList
      part2 = sum [ x * M.findWithDefault 0 x freqRight | x <- leftList ]

  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

frequencyMap :: [Int] -> Map Int Int
frequencyMap xs = M.fromListWith (+) [(x,1) | x <- xs]
