module Day02 (main02) where

import Util

between a b c = c >= min a b && c <= max a b

solveA input = listCount f input
  where
    diffs = \l -> zipWith (-) l (tail l)
    f l = (all (between 1 3) (diffs l)) || (all (between (-1) (-3)) (diffs l))

solveB input = listCount g input
  where
    diffs = \l -> zipWith (-) l (tail l)
    f l = (all (between 1 3) (diffs l)) || (all (between (-1) (-3)) (diffs l))
    g l = any f [ (take x l) ++ (drop (x+1) l) | x <- [0 .. length l] ] 

main02 :: IO ()
main02 = do
    input <- map (map read . splitOn " ") . lines <$> readFile "res/input02" :: IO [[Int]]
    print $ solveA input
    print $ solveB input
