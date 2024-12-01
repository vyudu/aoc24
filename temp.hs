import System.IO
import Data.List

main = do
    handle <- openFile "input" ReadMode
    input <- hGetContents handle
    let rows = lines input
    putStrLn "Part 1:"
    putStrLn "Part 2:"

