import System.IO
import Data.Char (isSpace)
import qualified Data.Set as S

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

directions :: [(Int,Int)]
directions = [(0,-1),(1,0),(0,1),(-1,0)] -- Up, Right, Down, Left

parseMap :: FilePath -> IO ([String], (Int,Int), (Int,Int))
parseMap filePath = do
  grid <- map trim . lines <$> readFile filePath
  let (sp, d, g2) = findStart grid
  return (g2, sp, d)

findStart :: [String] -> ((Int, Int),(Int, Int),[String])
findStart g =
  let rows = length g
      cols = if rows == 0 then 0 else length (head g)
      dirMap = [ ('^',(0,-1)),('v',(0,1)),('<',(-1,0)),('>',(1,0)) ]
      [(sx,sy,dx,dy)] = 
        [ (x,y,dx,dy) | y <- [0..rows-1], x <- [0..cols-1]
                      , let c = (g !! y) !! x
                      , (ch,(dx,dy)) <- dirMap, c == ch ]
      g2 = replaceChar g (sx, sy) '.'
  in ((sx,sy),(dx,dy),g2)

replaceChar :: [String] -> (Int, Int) -> Char -> [String]
replaceChar g (x,y) c =
  let row = g !! y
      newRow = take x row ++ [c] ++ drop (x+1) row
  in take y g ++ [newRow] ++ drop (y+1) g

simulatePatrol :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
simulatePatrol g (sx,sy) dirVec =
  let rows = length g
      cols = if rows == 0 then 0 else length (head g)
      cdir = directionIndex dirVec
      step (x,y,c,vis) =
        let (dx,dy) = directions !! c
            (nx,ny) = (x+dx,y+dy)
        in if nx<0 || ny<0 || ny>=rows || nx>=cols then vis
            else if (g !! ny) !! nx /= '#' then step (nx,ny,c,(nx,ny):vis)
            else step (x,y,(c+1) `mod` 4,(x,y):vis)
  in removeDups (step (sx,sy,cdir,[(sx,sy)]))

removeDups :: Eq a => [a] -> [a]
removeDups = go []
  where go _   []     = []
        go acc (z:zs) = if z `elem` acc then go acc zs else z : go (z:acc) zs

directionIndex :: (Int, Int) -> Int
directionIndex d = case lookup d (zip directions [0..]) of
                     Just i -> i
                     _ -> 0

simulatePatrolWithObstruction :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
simulatePatrolWithObstruction g (sx,sy) dirVec (ox,oy) =
  let g2 = replaceChar g (ox,oy) '#'
    rows = length g2
    cols = if rows == 0 then 0 else length (head g2)
    cdir = directionIndex dirVec
    stepLimit = rows * cols * 4
    go visited (x,y,c,steps)
      | steps > stepLimit = False
      | (x,y,c) `S.member` visited = True
      | otherwise =
          let visited' = S.insert (x,y,c) visited
              (dx,dy) = directions !! c
              (nx,ny) = (x+dx,y+dy)
          in if nx<0 || ny<0 || ny>=rows || nx>=cols then False
              else if (g2 !! ny) !! nx /= '#'
                  then go visited' (nx,ny,c,steps+1)
                  else go visited' (x,y,(c+1) `mod` 4,steps+1)
  in go S.empty (sx,sy,cdir,0)

findValidObstructions :: [String] -> (Int, Int) -> (Int, Int) -> Int
findValidObstructions g (sx,sy) _
  | null g = 0
  | otherwise =
    let rows = length g
        cols = length (head g)
        candidates = [ (x,y) | y <- [0..rows-1], x <- [0..cols-1]
                              , (x,y) /= (sx,sy), (g !! y) !! x == '.' ]
        results = filter (\pos -> simulatePatrolWithObstruction g (sx,sy) (dx,dy) pos) candidates
    in length results
  where (dx,dy) = head [ d | d <- directions, directionIndex d == directionIndex (dx,dy) ]

main :: IO ()
main = do
  (grid, startPos, direction) <- parseMap "input/input_6.txt"
  let visitedPositions = simulatePatrol grid startPos direction
  let part1Result = length visitedPositions
  let part2Result = findValidObstructions grid startPos direction
  putStrLn $ "Part 1:" ++ show part1Result
  putStrLn $ "Part 2:" ++ show part2Result
