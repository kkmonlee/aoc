-- ghc -O2 -package containers -o 12 12.hs
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.IntMap.Strict as IM

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

readInt :: String -> Maybe Int
readInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

parseShapeHeader :: String -> Maybe Int
parseShapeHeader s =
  let t = trim s
      (ds, rest) = span isDigit t
  in if not (null ds) && trim rest == ":"
       then readInt ds
       else Nothing

parseRegionLine :: String -> Maybe (Int, Int, [Int])
parseRegionLine s
  | null (trim s) = Nothing
  | otherwise =
      let t = trim s
          (lhs, rest) = break (== ':') t
      in case rest of
           ':' : rhs ->
             let (wStr, xh) = break (== 'x') lhs
             in case xh of
                  'x' : hStr -> do
                    w <- readInt (trim wStr)
                    h <- readInt (trim hStr)
                    counts <- traverse readInt (words (trim rhs))
                    pure (w, h, counts)
                  _ -> Nothing
           _ -> Nothing

countHashes :: [String] -> Int
countHashes = sum . map (length . filter (== '#'))

parseShapes :: [String] -> (IM.IntMap Int, [String])
parseShapes = go IM.empty
  where
    isRegion s = case parseRegionLine s of Just _ -> True; Nothing -> False

    go m [] = (m, [])
    go m (l:ls)
      | null (trim l) = go m ls
      | isRegion l    = (m, l:ls)
      | otherwise =
          case parseShapeHeader l of
            Nothing  -> go m ls
            Just idx ->
              let isGridLine x =
                    let t = trim x
                    in not (null t) && parseShapeHeader x == Nothing && not (isRegion x)
                  (grid, rest) = span isGridLine ls
                  area = countHashes (map trim grid)
              in go (IM.insert idx area m) rest

main :: IO ()
main = do
  content <- readFile "input/input_12.txt"
  let ls = lines content
      (areas, regionLines) = parseShapes ls
      fits = sum
        [ 1
        | line <- regionLines
        , Just (w, h, counts) <- [parseRegionLine line]
        , let req = sum (zipWith (\i q -> q * IM.findWithDefault 0 i areas) [0..] counts)
        , req <= w * h
        ]
  print fits
