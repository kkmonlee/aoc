{-# LANGUAGE BangPatterns, StrictData #-}
import System.Environment (getArgs)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.List (foldl')
import Control.DeepSeq (force)

data Point = P !Int !Int deriving (Eq, Ord)
type Grid = V.Vector (UV.Vector Char)
type Region = S.Set Point

toGrid :: [String] -> Grid
toGrid = V.fromList . map UV.fromList

inBounds :: Int -> Int -> Point -> Bool
inBounds !h !w (P x y) = x >= 0 && y >= 0 && x < w && y < h
{-# INLINE inBounds #-}

neighbors :: Point -> [Point]
neighbors (P x y) = map (uncurry P) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
{-# INLINE neighbors #-}

findRegions :: Grid -> [(Char, Region)]
findRegions grid = filter (not . S.null . snd) $ map makeRegion points
  where
    !h = V.length grid
    !w = UV.length (grid V.! 0)
    points = [(P x y, (grid V.! y) UV.! x) | y <- [0..h-1], x <- [0..w-1]]
    
    makeRegion (!p, !c) = (c, flood p c S.empty)
    
    flood :: Point -> Char -> Region -> Region
    flood !p !c !seen
      | not (inBounds h w p) = seen
      | p `S.member` seen = seen
      | (grid V.! y) UV.! x /= c = seen
      | otherwise = foldl' (\s n -> flood n c s) (S.insert p seen) (neighbors p)
      where
        P x y = p
{-# INLINE flood #-}

price1 :: Region -> Int
price1 !r = S.size r * perimeter
  where
    perimeter = sum [1 | p <- S.toList r, n <- neighbors p, not $ n `S.member` r]

price2 :: Region -> Int
price2 !r = S.size r * perimeter
  where
    perimeter = sum [1 | p@(P x y) <- S.toList r
                      , (n,p1,p2) <- checks x y
                      , not (n `S.member` r)
                      , not (p1 `S.member` r && not (p2 `S.member` r))]
    
    checks !x !y = [ (P (x+dx) (y+dy), P (x+x1) (y+y1), P (x+x2) (y+y2))
                   | ((dx,dy),(x1,y1),(x2,y2)) <- 
                     [((1,0),(0,-1),(1,-1))
                     ,((-1,0),(0,-1),(-1,-1))
                     ,((0,1),(-1,0),(-1,1))
                     ,((0,-1),(-1,0),(-1,-1))]]
{-# INLINE checks #-}

main :: IO ()
main = do
  args <- getArgs
  let file = if null args then "input/input_12.txt" else head args
  contents <- readFile file
  let !grid = force $ toGrid $ lines contents
      !regions = findRegions grid
  print $ sum [price1 reg | (_,reg) <- regions]
  print $ sum [price2 reg | (_,reg) <- regions]