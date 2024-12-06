import System.IO
import Data.List

main = do
    contents <- lines <$> readFile "input"
    putStrLn "Part 1:"
    print $ countXMAS contents
    putStrLn "Part 2:"
    print $ countX_MAS contents

data Shift = L | R

countXMAS :: [[Char]] -> Int
countXMAS grid = 
    let r = length (head grid)
        c = length (grid)
        lgrid = cycleLeft grid
        rgrid = cycleRight grid
        lds = filter isWordP1 $ map (vertSlice 4 lgrid) [shiftIdx L r (x,y) | x <- [0..r-4], y <- [0..c-1]]
        rds = filter isWordP1 $ map (vertSlice 4 rgrid) [shiftIdx R r (x,y) | x <- [3..r-1], y <- [0..c-1]]
        vs = filter isWordP1 $ map (vertSlice 4 grid) [(x,y) | x <- [0..r-1], y <- [0..c-1]]
        hs = filter isWordP1 $ map (horiSlice 4 grid) [(x,y) | x <- [0..r-1], y <- [0..c-1]]
    in sum $ map length [lds, rds, vs, hs]

isWordP1 :: Maybe [Char] -> Bool
isWordP1 str = case str of
    Nothing -> False
    Just str -> (str == "XMAS" || str == "SAMX")

-- Get vertical slice at (x, y)
vertSlice :: Int -> [[Char]] -> (Int,Int) -> Maybe [Char]
vertSlice n grid (x,y) 
    | y > (length grid - n) = Nothing
    | otherwise = Just (map (!! x) rows)
    where rows = take n (drop y grid)

horiSlice :: Int -> [[Char]] -> (Int,Int) -> Maybe [Char]
horiSlice n grid (x,y) 
    | x > (length (head grid) - n) = Nothing
    | otherwise = Just (take n (drop x row))
    where row = head (drop y grid)

cycleLeft :: [[Char]] -> [[Char]] 
cycleLeft grid = zipWith cycleBy [0..] grid

cycleRight :: [[Char]] -> [[Char]]
cycleRight grid = zipWith cycleBy [n,n-1..] grid 
    where n = length (head grid)

cycleBy :: Int -> [a] -> [a]
cycleBy n xs = take (length xs) (drop n $ cycle xs)

-- Part 2
isWord :: Maybe [Char] -> Bool
isWord str = case str of
    Nothing -> False
    Just str -> (str == "MAS" || str == "SAM") 

-- Given a shift direction, a row length, and an index, give index in shifted array
shiftIdx :: Shift -> Int -> (Int, Int) -> (Int, Int)
shiftIdx sh r (x,y) = case sh of
    L -> ((r+x-y) `mod` r, y)
    R -> ((x+y) `mod` r, y)

isX_MAS :: Int -> Int -> [[Char]] -> Bool
isX_MAS x y grid 
    | (x == 0 || y == 0) = False
    | (x == r-1 || y == c-1) = False 
    | grid !! y !! x /= 'A' = False 
    | otherwise = (isWord $ vertSlice 3 lgrid lidx) && (isWord $ vertSlice 3 rgrid ridx)
    where r = length (head grid)
          c = length grid 
          lgrid = cycleLeft grid
          rgrid = cycleRight grid
          lidx = shiftIdx L r (x-1, y-1) 
          ridx = shiftIdx R r (x+1, y-1) 

countX_MAS :: [[Char]] -> Int
countX_MAS grid = length $ filter (==True) [isX_MAS x y grid | x <- [0..r], y <- [0..c]]
    where r = length (head grid) - 1
          c = length grid - 1
