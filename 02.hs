import System.IO
import Data.List

main = do
    levels :: [[Int]] <- map ((map read) . words) <$> lines <$> readFile "input"
    putStrLn "Part 1:"
    print $ length $ filter (== True) $ map isStable levels 
    putStrLn "Part 2:"
    print $ length $ filter (== True) $ map isDampStable levels 

stableDiff :: Int -> Int -> Bool
stableDiff sgn x = sgn * x > 0 && sgn * x < 4

isStable :: [Int] -> Bool
isStable (x:y:xs) = 
    let sgn = signum $ (x - y)
    in all (stableDiff sgn) $ zipWith (-) (x:y:xs) (y:xs)

isDampStable :: [Int] -> Bool
isDampStable (x:y:xs)
    | length badlevels == 0 = True
    | length badlevels == 1 = True
    | length baddiffs == 1 = any (\(x,y) -> stableDiff sgn $ uncurry (+) (x,y)) badlevels
    | length baddiffs == 2 = case (elem (head baddiffs, last baddiffs) badlevels) of 
        True -> stableDiff sgn (sum baddiffs)
        False -> False
    | otherwise = False
    where diffs = zipWith (-) (x:y:xs) (y:xs)
          sgn = signum $ sum (map signum diffs)
          baddiffs = filter (not . stableDiff sgn) diffs
          badlevels = filter (\(x,y) -> (not $ stableDiff sgn x) || (not $ stableDiff sgn y)) (zip diffs (tail diffs))
