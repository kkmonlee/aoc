import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (find)

type Point = (Int, Int)
type Grid = [[Char]]

data Direction = Up | Right | Down | Left deriving (Enum, Show)
type Vector = (Int, Int)

directions :: [(Direction, Vector)]
directions = [(Up, (-1,0)), (Right, (0,1)), (Down, (1,0)), (Left, (0,-1))]

addPoints :: Point -> Vector -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

inBounds :: Grid -> Point -> Bool
inBounds grid (r, c) = r >= 0 && r < length grid && c >= 0 && c < length (head grid)

findStart :: Grid -> Point
findStart grid = head [(r,c) | (r,row) <- zip [0..] grid, (c,ch) <- zip [0..] row, ch == 'S']

replaceStart :: Grid -> Point -> (Grid, Char)
replaceStart grid pos@(r,c) = (replaceAt pos replacement grid, replacement)
  where
    checkDir (dir, vec) = inBounds grid p && connecting
      where
        p = addPoints pos vec
        ch = (grid !! fst p) !! snd p
        connecting = case dir of
            Up    -> ch `elem` "|7F"
            Right -> ch `elem` "-7J"
            Down  -> ch `elem` "|LJ"
            Left  -> ch `elem` "-LF"
    valid = map (checkDir) directions
    replacement = case valid of
        [True,  True,  False, False] -> 'L'
        [True,  False, False, True ] -> 'J'
        [False, True,  True,  False] -> 'F'
        [False, False, True,  True ] -> '7'
        [True,  False, True,  False] -> '|'
        [False, True,  False, True ] -> '-'
        _ -> error "Invalid start configuration"

replaceAt :: Point -> a -> [[a]] -> [[a]]
replaceAt (r,c) x = modifyAt r (modifyAt c (const x))

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

expandGrid :: Grid -> Grid
expandGrid grid = [[expandCell r c | c <- [0..width-1], ec <- [0..2]]
                                 | r <- [0..height-1], er <- [0..2]]
  where
    height = length grid
    width = length (head grid)
    expandCell r c = case (grid !! r) !! c of
        '|' -> if ec == 1 then 'x' else '.'
        '-' -> if er == 1 then 'x' else '.'
        'L' -> if (er == 0 && ec == 1) || (er == 1 && ec > 0) then 'x' else '.'
        'J' -> if (er == 0 && ec == 1) || (er == 1 && ec < 2) then 'x' else '.'
        '7' -> if (er == 2 && ec == 1) || (er == 1 && ec < 2) then 'x' else '.'
        'F' -> if (er == 2 && ec == 1) || (er == 1 && ec > 0) then 'x' else '.'
        _   -> '.'

floodFill :: Grid -> Set.Set Point -> [Point] -> Set.Set Point
floodFill _ seen [] = seen
floodFill grid seen (p:ps)
    | not (inBounds grid p) = floodFill grid seen ps
    | p `Set.member` seen = floodFill grid seen ps
    | (grid !! fst p) !! snd p == 'x' = floodFill grid seen ps
    | otherwise = floodFill grid seen' (ps ++ neighbors)
    where
        seen' = Set.insert p seen
        neighbors = [addPoints p vec | (_, vec) <- directions]

countInside :: Grid -> Grid -> Int
countInside orig expanded = sum [1 | r <- [0..height-1], 
                                   c <- [0..width-1], 
                                   not (any (\(er,ec) -> Set.member (3*r+er,3*c+ec) outside) 
                                   [(er,ec) | er <- [0..2], ec <- [0..2]])]
  where
    height = length orig
    width = length (head orig)
    edges = [(r,0) | r <- [0..length expanded - 1]] ++
            [(r,length (head expanded) - 1) | r <- [0..length expanded - 1]] ++
            [(0,c) | c <- [0..length (head expanded) - 1]] ++
            [(length expanded - 1,c) | c <- [0..length (head expanded) - 1]]
    outside = floodFill expanded Set.empty edges

main :: IO ()
main = do
    content <- readFile "input/input_10.txt"
    let grid = lines content
    let start = findStart grid
    let (grid', _) = replaceStart grid start
    let expanded = expandGrid grid'
    
    putStrLn $ show $ length grid `div` 2
    putStrLn $ show $ countInside grid expanded