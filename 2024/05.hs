import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Unique qualified as D
import System.IO

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  content <- readFile "input/input_5.txt"
  let [rulesSec, updatesSec] = splitOn "\n\n" (trim content)
  let rules = map parseRule (lines rulesSec)
      updates = map (map read . splitOn ",") (lines updatesSec)

  let correctUpdates = filter (isUpdateOrdered rules) updates
      correctMiddleSum = sum (map middlePage correctUpdates)

  let incorrectUpdates = filter (not . isUpdateOrdered rules) updates
      reordered = map (topologicalSortUpdate rules) incorrectUpdates
      incorrectMiddleSum = sum (map middlePage reordered)

  putStrLn $ "Part 1: " ++ show correctMiddleSum
  putStrLn $ "Part 2: " ++ show incorrectMiddleSum

parseRule :: String -> (Int, Int)
parseRule line =
  let [x, y] = splitOn "|" line
   in (read x, read y)

isUpdateOrdered :: [(Int, Int)] -> [Int] -> Bool
isUpdateOrdered rs upd =
  let indexMap = M.fromList (zip upd [0 ..])
   in all (checkOrder indexMap) rs
  where
    checkOrder idxMap (x, y) =
      case (M.lookup x idxMap, M.lookup y idxMap) of
        (Just ix, Just iy) -> ix < iy
        _ -> True

middlePage :: [Int] -> Int
middlePage upd = upd !! (length upd `div` 2)

topologicalSortUpdate :: [(Int, Int)] -> [Int] -> [Int]
topologicalSortUpdate rs upd =
  let nodes = S.fromList upd
      applicable = [(x, y) | (x, y) <- rs, x `S.member` nodes, y `S.member` nodes]

      (graph, inDeg) = buildGraph applicable nodes
   in -- topological sort
      topoSort graph inDeg (S.toList nodes)

buildGraph :: [(Int, Int)] -> S.Set Int -> (M.Map Int [Int], M.Map Int Int)
buildGraph edges nodes =
  let initialInDeg = M.fromList [(n, 0) | n <- S.toList nodes]
      graphBuild =
        foldl
          ( \(g, deg) (x, y) ->
              let g' = M.insertWith (++) x [y] g
                  deg' = M.insertWith (+) y 1 deg
               in (g', deg')
          )
          (M.empty, initialInDeg)
          edges
   in graphBuild

topoSort :: M.Map Int [Int] -> M.Map Int Int -> [Int] -> [Int]
topoSort graph inDeg allNodes =
  let zeroDeg = [n | n <- allNodes, M.findWithDefault 0 n inDeg == 0]
      q = D.fromList zeroDeg
   in go q inDeg []
  where
    go q deg result
      | D.null q = result
      | otherwise =
          let (current, q') = D.popFront q
              nexts = M.findWithDefault [] current graph
              (q'', deg') = foldl reduceDeg (q', deg) nexts
           in go q'' deg' (result ++ [current])

    reduceDeg (q, deg) v =
      let d = M.findWithDefault 0 v deg - 1
          deg' = M.insert v d deg
       in if d == 0 then (D.pushBack q v, deg') else (q, deg')
