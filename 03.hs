import System.IO
import Data.List
import Text.Regex.PCRE

main = do
    contents <- lines <$> readFile "input"
    putStrLn "Part 1:"
    print $ sum $ map getProds contents
    putStrLn "Part 2:"
    print $ getActivatedProds (foldl (++) "" contents)

mulregex = "mul\\(([0-9]+),([0-9]+)\\)"

getProds :: String -> Int
getProds s = 
    let nums :: [[String]] = (s =~ mulregex)
    in sum $ map product $ map (map read . tail) nums

activatedRegex = "^(.*?)don't\\(\\)|do\\(\\)(.*?)don't\\(\\)|do\\(\\)(.*?)$"

getActivatedProds :: String -> Int
getActivatedProds s = 
    let activeStr :: [String] = getAllTextMatches (s =~ activatedRegex)
        nums :: [[String]] = (=~ mulregex) =<< activeStr
    in sum $ map product $ map (map read . tail) nums
