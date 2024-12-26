module Day08 (main08) where

import Util
import qualified Data.Map as Map
import qualified Data.Array.IArray as Arr

solve nodes mat =
  mat & Arr.assocs
      & filter ((/= '.') . snd)
      & map (\(x, y) -> (y, [x]))
      & Map.fromListWith (++)
      & Map.assocs
      & concatMap (snd >>> makePairs >>> concatMap (nodes mat))
      & mkUniq
      & length

nodesA mat (x, y) = filter (inBounds (Arr.bounds mat)) [ x - (y - x), y - (x - y) ]

nodesB mat (x, y) = f y dir ++ f x (-dir)
  where
    V2 dx' dy' = y - x
    g = gcd dx' dy'
    dir = V2 (dx' `div` g) (dy' `div` g)
    f start dir = takeWhile (inBounds (Arr.bounds mat)) $ iterate (+dir) start


main08 :: IO ()
main08 = do
    input <- parseMatrix <$> readFile "res/input08"
    print $ solve nodesA input
    print $ solve nodesB input
