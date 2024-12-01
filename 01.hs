import System.IO
import Data.List

main = do
    handle <- openFile "input" ReadMode
    input <- hGetContents handle
    let rows = lines input
    putStrLn "Part 1:"
    print $ sortedDiff (map getList1 rows) (map getList2 rows)
    putStrLn "Part 2:"
    print $ similarity (map getList1 rows) (map getList2 rows)

getList1 :: String -> Integer
getList1 = read . head . words

getList2 :: String -> Integer
getList2 = read . last . words

sortedDiff :: [Integer] -> [Integer] -> Integer
sortedDiff l1 l2 = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

similarity :: [Integer] -> [Integer] -> Integer
similarity l1 l2 = 
    let sl1 = sort l1
        sl2 = sort l2
    in sum $ zipWith (*) (sl1) (counts sl1 sl2)

counts :: [Integer] -> [Integer] -> [Integer]
counts l1 [] = [0]
counts [] l2 = []
counts l1 l2 = 
    let h = head l1
        _l1 = dropWhile (==h) l1 
        _l2 = dropWhile (<h) l2 
        __l2 = dropWhile (==h) _l2 
        count = toInteger (length _l2 - length __l2)
        rpts = length l1 - length _l1
    in replicate rpts count ++ (counts _l1 __l2)
