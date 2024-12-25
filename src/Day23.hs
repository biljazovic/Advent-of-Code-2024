module Day23 (main23) where

import Util
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array ( Array )
import qualified Data.Array as Arr
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Array.Unboxed as UA
import Data.Bits

makeGraph input = (n, vertices, edges, indexMapR)
  where
    edges' = concatMap ((\[a, b] -> [(a, b), (b, a)]) . splitOn "-") input
    n = length vertices - 1
    vertices = nubOrd $ concatMap (splitOn "-") input
    indexMap = Map.fromList $ zipWith (,) vertices [0..]
    indexMapR = Map.fromList $ map swap $ Map.assocs indexMap
    edges = indexize edges'
    indexize = map (\(a, b) -> (indexMap Map.! a, indexMap Map.! b))


solveA input = listCount good . nubOrd . concatMap (map sort . cycles) $ edges
  where
    (n, vertices, edges, indexMapR) = makeGraph input
    adj :: UA.UArray (Int, Int) Bool
    adj = UA.accumArray (||) False ((0, 0), (n, n))
          $ map (, True) edges
    cycles (x, y) = map (\z -> [x, y, z]) $ filter (f x y) [0..n]
    f x y z = (adj UA.! (x, z)) && (adj UA.! (y, z))
    good = any ((== 't') . head . (indexMapR Map.!))

solveB input = concat $ intersperse "," $ sort $ map (indexMapR Map.!) xs
  where
    (n, vertices, edges, indexMapR) = makeGraph input

    Just (xs, _) = find good $ foldl' g [] [0..n]
    good (xs, adj) = length xs == popCount adj

    g :: [([Int], Integer)] -> Int -> [([Int], Integer)]
    g groups y = let
      groups' = sortOn ((0-) .cmp) groups
      adjy = adj Arr.! y
      cmp (_, g1) = popCount (g1 .&. adjy)
        in if null groups || (cmp (head groups') < 5)
              then ([y], adjy) : groups
              else let (xs, g):rest = groups'
                    in (y:xs, g .&. adjy) : rest

    adj :: Array Int Integer
    adj = foldl' f adj0 edges

    adj0 :: Array Int Integer
    adj0 = Arr.listArray (0, n) [ bit i | i <- [0..n] ]

    f :: Array Int Integer -> (Int, Int) -> Array Int Integer
    f adj' (a, b) = Arr.accum (.|.) adj' [(a, bit b), (b, bit a)]

main23 :: IO ()
main23 = do
    input <- lines <$> readFile "res/input23"
    print $ solveA input
    print $ solveB input
