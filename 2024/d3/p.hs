import System.IO
import Data.Char (isDigit)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (part1Sum, _) = parseInstructions input True False
  let (part2Sum, _) = parseInstructions input True True
  putStrLn $ "Part 1: " ++ show part1Sum
  putStrLn $ "Part 2: " ++ show part2Sum

parseInstructions :: String -> Bool -> Bool -> (Integer, Bool)
parseInstructions str startEnabled part2 = go str 0 startEnabled 0
  where
    go s i enabled acc
      | i >= length s = (acc, enabled)
      | otherwise =
          let c = s !! i
          in case c of
               'm' ->
                 if match "mul(" s i then
                   let i' = i + 4
                       (mx,i1) = readNumber s i'
                       ok1 = mx >= 0 && mx <= 999
                   in if not ok1 || i1 >= length s || i1 >= length s || (i1 >= length s || s !! i1 /= ',')
                      then go s (i+1) enabled acc
                      else
                        let (my,i2) = readNumber s (i1+1)
                            ok2 = my >= 0 && my <= 999
                        in if not ok2 || i2 >= length s || s !! i2 /= ')'
                           then go s (i+1) enabled acc
                           else
                             let res = if enabled then acc + fromIntegral (mx*my) else acc
                             in go s (i2+1) enabled res
                 else go s (i+1) enabled acc
               'd' ->
                 if part2 && match "do()" s i then
                   go s (i+4) True acc
                 else if part2 && match "don't()" s i then
                   go s (i+7) False acc
                 else go s (i+1) enabled acc
               _ -> go s (i+1) enabled acc

-- match pattern at index
match :: String -> String -> Int -> Bool
match pat s pos =
  let plen = length pat
      slen = length s
  in pos + plen <= slen && take plen (drop pos s) == pat

readNumber :: String -> Int -> (Int, Int)
readNumber s pos =
  let (digits, rest) = span isDigit (drop pos s)
  in if null digits || length digits > 3 then (-1, pos)
     else (read digits, pos + length digits)
