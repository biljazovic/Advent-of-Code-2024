module Day22 (main22) where

import Util
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Bits (xor)
import Data.Containers.ListUtils (nubIntOn)

moD = 16777216 :: Integer

next = step1 >>> step2 >>> step3
  where
    step1 x = ((x * 64) `xor` x) `mod` moD
    step2 x = ((x `div` 32) `xor` x) `mod` moD
    step3 x = ((x * 2048) `xor` x) `mod` moD

solveA = sum . map f
  where
    f = (!! 2000) . iterate next

changeId :: [Int] -> Int
changeId = foldr (\a b -> b * 20 + (a + 9)) 0

changes :: [Integer] -> [(Int, Integer)]
changes xs = chs
  where
    dfs = zipWith (-) (drop 1 xs) xs
    chs = map f $ zip5 dfs (drop 1 dfs) (drop 2 dfs) (drop 3 dfs) (drop 4 xs)
    f (x, y, z, w, a) = (changeId (map fromInteger [x, y, z, w]), a)

solveB input = maximum $ IntMap.elems mapa
  where
    mapa = foldr process IntMap.empty input
    process x mapa' =
      let xs = take 2000 $ iterate next x
          chs = changes $ map (`mod` 10) xs
       in IntMap.fromListWith (+) $
         IntMap.assocs mapa' ++ nubIntOn fst chs

main22 :: IO ()
main22 = do
    input <- map read . lines <$> readFile "res/input22" :: IO [Integer]
    print $ solveB input
