import qualified Data.Set as Set

parseNumbers :: String -> [Int]
parseNumbers = map read . words

parseLine :: String -> ([Int], [Int])
parseLine line = 
    let allNums = parseNumbers line
        half = length allNums `div` 2
    in splitAt half allNums

calculatePoints :: ([Int], [Int]) -> Int
calculatePoints (winning, yours) =
    let matches = length $ Set.intersection (Set.fromList winning) (Set.fromList yours)
    in if matches > 0 then 2 ^ (matches - 1) else 0

calculateCopies :: [([Int], [Int])] -> [Int]
calculateCopies cards =
    go (replicate (length cards) 1) 0 cards
  where
    go acc _ [] = acc
    go acc i ((winning, yours):rest) =
        let matches = length $ Set.intersection (Set.fromList winning) (Set.fromList yours)
            currentCopies = acc !! i
            updatedAcc = foldl (\a j -> 
                if j < length acc
                then take j a ++ [a !! j + currentCopies] ++ drop (j + 1) a
                else a)
                acc
                [i + 1 .. i + matches]
        in go updatedAcc (i + 1) rest

main :: IO ()
main = do
    content <- readFile "input/input_04.txt"
    let cards = map parseLine $ lines content
    
    let points = sum $ map calculatePoints cards
    print points
    
    let copies = calculateCopies cards
    print $ sum copies