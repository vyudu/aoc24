import System.IO
import Data.List

main = do
    handle <- openFile "input" ReadMode
    input <- hGetContents handle
    let (l1, l2) = getLists $ lines input
    putStrLn "Part 1:"
    print $ sortedDiff l1 l2
    putStrLn "Part 2:"
    print $ similarity l1 l2

getLists :: [String] -> ([Int], [Int])
getLists [] = ([], [])
getLists rows = 
    let r = head rows
        [i1, i2] = map read $ words r
        _rows = tail rows
    in ((i1:fst (getLists _rows)), (i2:snd (getLists _rows))) 

sortedDiff :: [Int] -> [Int] -> Int
sortedDiff l1 l2 = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

similarity :: [Int] -> [Int] -> Int
similarity l1 l2 = 
    let sl1 = sort l1
        sl2 = sort l2
    in sum $ zipWith (*) (sl1) (counts sl1 sl2)

counts :: [Int] -> [Int] -> [Int]
counts l1 [] = [0]
counts [] l2 = []
counts l1 l2 = 
    let h = head l1
        _l1 = dropWhile (==h) l1 
        _l2 = dropWhile (<h) l2 
        __l2 = dropWhile (==h) _l2 
        count = length _l2 - length __l2
        rpts = length l1 - length _l1
    in replicate rpts count ++ (counts _l1 __l2)
