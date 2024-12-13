import System.IO
import Data.Char (isSpace, isDigit, isAlphaNum)
import Control.Monad (guard)

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim xs =
    let (chunk, rest) = break (== delim) xs
    in chunk : case rest of
                 [] -> []
                 (_:r) -> splitOn delim r

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

evaluateExpression :: [Int] -> [String] -> Int
evaluateExpression (n:ns) ops = go n ns ops
  where
    go acc [] [] = acc
    go acc (x:xs) (o:os)
      | o == "+"  = go (acc + x) xs os
      | o == "-"  = go (acc - x) xs os
      | o == "*"  = go (acc * x) xs os
      | o == "||" = let newAcc = read (show acc ++ show x) :: Int
                    in go newAcc xs os
    go acc _ _ = acc
evaluateExpression [] _ = 0

isPossibleTarget :: Int -> [Int] -> [String] -> Bool
isPossibleTarget target numbers opList =
  let numPositions = length numbers - 1
      opsForAllPositions = sequence (replicate numPositions opList)
  in any (\comb -> evaluateExpression numbers comb == target) opsForAllPositions

calculateTotalCalibration :: FilePath -> [String] -> IO Integer
calculateTotalCalibration filePath operatorSet = do
  content <- readFile filePath
  let ls = lines content
  return $ sum $ map (processLine operatorSet) ls
where
  processLine opSet line =
    let line' = trim line
        (t:rest:_) = splitOn ':' line'
        target = read (trim t) :: Int
        nums = map (read . trim) (words (trim rest))
    in if isPossibleTarget target nums opSet
        then fromIntegral target
        else 0

main :: IO ()
main = do
  part1Result <- calculateTotalCalibration "input/input_7.txt" ["+","-","*"]

  part2Result <- calculateTotalCalibration "input/input_7.txt" ["+","-","*","||"]
  putStrLn $ "Part 1: " ++ show part1Result
  putStrLn $ "Part 2: " ++ show part2Result
