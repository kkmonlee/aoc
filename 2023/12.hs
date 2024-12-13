import qualified Data.Map as Map
import Data.List.Split (splitOn)

type Cache = Map.Map (Int, Int, Int) Integer
type Pattern = String
type Groups = [Int]

solve :: Pattern -> Groups -> Integer
solve pattern groups = go pattern groups 0 0 0 Map.empty
  where
    go :: Pattern -> Groups -> Int -> Int -> Int -> Cache -> Integer
    go pat grps i bi cur cache = case Map.lookup (i, bi, cur) cache of
        Just result -> result
        Nothing -> let result = calculate pat grps i bi cur
                      newCache = Map.insert (i, bi, cur) result cache
                  in result
    
    calculate :: Pattern -> Groups -> Int -> Int -> Int -> Integer
    calculate pat grps i bi cur
        | i == length pat = if bi == length grps && cur == 0 then 1
                          else if bi == length grps - 1 && grps !! bi == cur then 1
                          else 0
        | otherwise = sum $ do
            c <- if pat !! i == '?' then ['.', '#'] else [pat !! i]
            case c of
                '.' | cur == 0 -> [go pat grps (i+1) bi 0]
                    | cur > 0 && bi < length grps && grps !! bi == cur 
                        -> [go pat grps (i+1) (bi+1) 0]
                    | otherwise -> [0]
                '#' -> [go pat grps (i+1) bi (cur+1)]
                _   -> [0]

parseLine :: String -> (Pattern, Groups)
parseLine line = case words line of
    [pat, nums] -> (pat, map read $ splitOn "," nums)
    _           -> error "Invalid input format"

unfold :: (Pattern, Groups) -> (Pattern, Groups)
unfold (pat, grps) = 
    (intercalate "?" $ replicate 5 pat, concat $ replicate 5 grps)
  where
    intercalate sep = concat . (zipWith (++) ((""::String) : repeat sep))

main :: IO ()
main = do
    content <- readFile "input/input_12.txt"
    let input = map parseLine $ lines content
    
    print $ sum [solve pat grps | (pat, grps) <- input]
    
    print $ sum [solve pat grps | (pat, grps) <- map unfold input]