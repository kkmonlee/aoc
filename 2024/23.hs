{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (readFile)
import Data.List (sort, intercalate, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Foldable as F (foldl')

buildAdjacency :: [String] -> Map String (Set String)
buildAdjacency linesInput =
    let insertEdge m (a,b) =
            let m1 = M.insertWith S.union a (S.singleton b) m
            in  M.insertWith S.union b (S.singleton a) m1
        parseLine l =
          let (n1, '-':n2) = break (== '-') l
          in  (n1, n2)
    in  foldl insertEdge M.empty (map parseLine linesInput)

-- 3-computer connected sets
findTriangles :: Map String (Set String) -> [(String, String, String)]
findTriangles adj = 
    let nodes = sort (M.keys adj)
        n     = length nodes
        -- for each distinct triple (i<j<k), check if triangle
        triples = 
          [ (a, b, c)
          | i <- [0..(n-3)]
          , j <- [(i+1)..(n-2)]
          , k <- [(j+1)..(n-1)]
          , let a = nodes !! i
                b = nodes !! j
                c = nodes !! k
          , b `S.member` (adj M.! a)
          , c `S.member` (adj M.! a)
          , c `S.member` (adj M.! b)
          ]
    in  triples

-- | triangle nodes starting with t
countTrianglesWithT :: [(String, String, String)] -> Int
countTrianglesWithT = length . filter (\(a,b,c) ->
    any (\x -> take 1 x == "t") [a,b,c]
  )

-- node in p UNION x having intersections with p
findPivot :: Map String (Set String) -> Set String -> Set String -> String
findPivot adj p x =
    let px   = S.union p x
        -- get largest
        best = maximumBy (comparing (\u -> S.size (S.intersection (adj M.! u) p))) (S.toList px)
    in  best

bronKerboschMax
    :: Map String (Set String)
    -> Set String
    -> Set String
    -> Set String
    -> Set String
    -> Set String
bronKerboschMax adj r p x bestSoFar
    -- if P and X are empty, R is a maximal clique
    | S.null p && S.null x =
        if S.size r > S.size bestSoFar
           then r
           else bestSoFar
    | otherwise =
        -- pivot selection heuristic
        let pivot     = findPivot adj p x
            -- vertices in P \ neighbours(pivot)
            toExplore = p S.\\ (adj M.! pivot)
        in  S.foldl'
              (\currentBest v ->
                  let r' = S.insert v r
                      p' = p `S.intersection` (adj M.! v)
                      x' = x `S.intersection` (adj M.! v)
                      newBest = bronKerboschMax adj r' p' x' currentBest
                      -- remove v from p and add to x
                  in  if S.size newBest > S.size currentBest
                         then newBest
                         else currentBest
              )
              bestSoFar
              toExplore

-- largest clique in graph
findMaxClique :: Map String (Set String) -> Set String
findMaxClique adj =
    let allNodes   = M.keysSet adj
        best       = S.empty
    in  bronKerboschMax adj S.empty allNodes S.empty best

main :: IO ()
main = do
    input <- readFile "input/input_23.txt"
    let ls     = filter (not . null) . map (filter (/= '\r')) . lines $ input
        adj    = buildAdjacency ls

    -- p1
    let allTriangles   = findTriangles adj
        countWithT     = countTrianglesWithT allTriangles
    print countWithT

    -- p2
    let largestClique  = findMaxClique adj
        sortedMembers  = sort (S.toList largestClique)
        password       = intercalate "," sortedMembers
    putStrLn password
