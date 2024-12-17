import System.IO

main :: IO ()
main = do
  text <- lines <$> readFile "input/input_4.txt"
  let height = length text
      width = length (head text)
      directions = [(1,0),(0,1),(1,1),(-1,1),(1,-1),(-1,-1),(0,-1),(-1,0)]

      inBounds i j = i >= 0 && j >= 0 && i < height && j < width
      charAt i j = (text !! i) !! j

      checkMatch i j dx dy =
        let indices = [(i + dx*k, j + dy*k) | k <- [0..3]]
        in all (uncurry inBounds) indices &&
            map (uncurry charAt) indices == "XMAS"

      matches = length [() | i <- [0..height-1], j <- [0..width-1], (dx,dy) <- directions, checkMatch i j dx dy]

      isCrossMas i j =
        inBounds i j &&
        charAt i j == 'A' &&
        inBounds (i+1) (j+1) &&
        inBounds (i-1) (j-1) &&
        inBounds (i+1) (j-1) &&
        inBounds (i-1) (j+1) &&
        let diag1 = [charAt (i+1) (j+1), charAt (i-1) (j-1)]
            diag2 = [charAt (i+1) (j-1), charAt (i-1) (j+1)]
        in (sorted diag1 == "MS") && (sorted diag2 == "MS")

      sorted xs = let s = quicksort xs in s
      quicksort [] = []
      quicksort (p:xs) = quicksort [x|x<-xs,x<=p]++[p]++quicksort [x|x<-xs,x>p]

      count = length [() | i <- [0..height-1], j <- [0..width-1], isCrossMas i j]
  putStrLn $ "Part 1: " ++ show matches
  putStrLn $ "Part 2: " ++ show count
