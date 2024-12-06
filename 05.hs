import System.IO
import Data.List
import Data.List.Split
import Data.Graph
import Data.Map (Map, (!), fromList, size)
import qualified Data.Map as Map

main = do
    contents <- lines <$> readFile "input"
    let (rules, lists) = parseInput contents
    let rg = ruleGraph rules
    putStrLn "Part 1:"
    print $ sum $ map middle $ filter (isValid rg) lists
    putStrLn "Part 2:"
    print $ sum $ map (middle . sortBy (ruleCmp rg)) (filter (not . isValid rg) lists)

data RuleGraph = RuleGraph {graph :: Graph, nummap :: Map Int Int}

middle xs = xs !! (length xs `div` 2)

nodeMap :: [[Int]] -> Map Int Int
nodeMap nums = fromList $ zip (nub $ concat nums) [0..]

parseInput :: [String] -> ([[Int]], [[Int]])
parseInput strs = 
    let rules :: [[Int]] = map (map read . splitOn "|") $ takeWhile (not . null) strs
        lists :: [[Int]] = map (map read . splitOn ",") $ tail (dropWhile (not . null) strs)
    in (rules, lists)

ruleGraph :: [[Int]] -> RuleGraph
ruleGraph rules = 
    let numMap = nodeMap rules
        pairs :: [(Int, Int)] = map (\x -> (numMap ! head x, numMap ! last x)) rules
    in RuleGraph (buildG (0, size numMap) pairs) numMap

borders :: RuleGraph -> (Int, Int) -> Bool
borders g (x,y) = ((nummap g) ! x, (nummap g) ! y) `elem` edges (graph g)

isValid :: RuleGraph -> [Int] -> Bool
isValid g list = foldl (\val (x,y) -> val && borders g (x,y)) True (zip list (tail list))
    
ruleCmp :: RuleGraph -> Int -> Int -> Ordering
ruleCmp rg x y = case borders rg (x,y) of 
    True -> LT
    False -> GT
