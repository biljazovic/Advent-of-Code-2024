module Day04 (main04) where

import Util
import qualified Data.Array as Arr

isXMAS mat pos dir = (==) "XMAS"
  [ arrLookupWithDefault '\0' mat (pos + scale d dir) | d <- [0..3] ]

countXMAS mat pos = listCount (isXMAS mat pos) [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1), V2 1 1, V2 (-1) 1, V2 1 (-1), V2 (-1) (-1)]

solveA mat = sum $ map (countXMAS mat) $ Arr.indices mat

solveB mat = listCount f $ Arr.indices mat
  where
    f pos = all (`elem` ["MAS", "SAM"]) $ map (g pos) [V2 1 1, V2 1 (-1)]
    g pos dir = [ arrLookupWithDefault '\0' mat (pos + scale d dir) | d <- [-1..1] ]

main04 :: IO ()
main04 = do
    input <- parseMatrix <$> readFile "res/input04"
    print $ solveA input
    print $ solveB input
