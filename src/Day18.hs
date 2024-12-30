module Day18 (main18) where

import Util
import Data.Maybe
import qualified Data.Array.IArray as Arr

solve input = dijkstra [ V2 0 0 ] (== (V2 70 70)) neigh
  where
    mat :: CharMatrix
    mat = Arr.accumArray (\x y -> y) '.' (V2 0 0, V2 70 70) [ (V2 x y, '#') | (y, x) <- input ]
    neigh p = map (, 1) $ filter ((== '.') . (arrLookupWithDefault '#' mat)) $ susedi4 Nothing p

solveB input = input !! (i-1)
  where
    i = binarySearch 1024 (length input) f
    f j = isNothing (solve (take j input))

main18 :: IO ()
main18 = do
    input <- map ((\[x, y] -> (read x, read y)) . splitOn ",") . lines <$> readFile "res/input18"
    print $ solve $ take 1024 input
    print $ solveB input
