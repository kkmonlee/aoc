{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Grid = [[Char]]
type Pos = (Int, Int)

main :: IO ()
main = do
  [grid, instrs] <- splitOn "\n\n" <$> readFile "input/input_15.txt"
  let g = lines grid
  print $ "Part 1: " ++ show (evalState (simulate g False instrs) g)
  print $ "Part 2: " ++ show (evalState (simulate g True instrs) g)

simulate :: Grid -> Bool -> String -> State Grid Int
simulate g part2 instrs = do
  grid <- return $ if part2 then expand g else g
  let (sr, sc) = head [(r, c) | (r, row) <- zip [0..] grid, (c, ch) <- zip [0..] row, ch == '@']
  modify $ update sr sc '.'
  foldM_ (step sr sc) instrs instrs
  gets score

expand :: Grid -> Grid
expand = map (concatMap (\c -> case c of '#' -> "##"; 'O' -> "[]"; '.' -> ".."; '@' -> "@."; _ -> ".."))

step :: Int -> Int -> Char -> Char -> State Grid ()
step r c dir _ = do
  let delta '^' = (-1, 0); delta 'v' = (1, 0); delta '<' = (0, -1); delta '>' = (0, 1)
      (dr, dc) = delta dir
  grid <- get
  case grid !! (r + dr) !! (c + dc) of
    '#' -> return ()
    '.' -> modify $ update (r + dr) (c + dc) '.'
    'O' -> push grid r c dr dc
    '[' -> push grid r c dr dc
    ']' -> push grid r c dr dc
    _   -> return ()

push :: Grid -> Int -> Int -> Int -> Int -> State Grid ()
push grid r c dr dc = do
  let bfs g p = bfs' (Seq.singleton p) Set.empty where
        bfs' Seq.Empty visited = Set.toList visited
        bfs' (p Seq.:<| q) visited
          | Set.member p visited = bfs' q visited
          | otherwise =
              let (r, c) = p; next = [(r + dr, c + dc) | grid !! (r + dr) !! (c + dc) /= '#']
              in bfs' (q Seq.>< Seq.fromList next) (Set.insert p visited)
      canMove ps = all (\(r, c) -> grid !! (r + dr) !! (c + dc) /= '#') ps
      ps = bfs grid (r, c)
  when (canMove ps) $
    modify $ foldl (\g (r, c) -> update (r + dr) (c + dc) (grid !! r !! c) $ update r c '.' g) grid ps

update :: Int -> Int -> Char -> Grid -> Grid
update r c val grid = take r grid ++ [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

score :: Grid -> Int
score g = sum [100 * r + c | (r, row) <- zip [0..] g, (c, ch) <- zip [0..] row, ch == 'O' || ch == '[']
