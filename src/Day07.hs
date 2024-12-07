module Day07 (main07) where

import Util

parse :: String -> (Integer, [Integer])
parse str = (x, ys)
  where
    [read -> x, ys'] = splitOn ": " str
    ys = map read $ splitOn " " ys'

solve ops = sum . map fst . filter f
  where
    f (x, ys) = elem x $ g (reverse ys)
    g [y] = [y]
    g (y : ys) = foldr1 (++) $ map (\op -> map (`op` y) (g ys)) ops

solveA = solve [ (+), (*) ]
solveB = solve [ (+), (*), comb ]
  where
    comb x y = read $ show x ++ show y

main07 :: IO ()
main07 = do
    input <- map parse . lines <$> readFile "res/input07"
    print $ solveA input
    print $ solveB input
