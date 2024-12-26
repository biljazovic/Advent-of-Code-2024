module Day20 (main20) where

import Util
import Data.Maybe
import qualified Data.Array.IArray as Arr
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)

astarDists :: (Eq a, Hashable a) => (a -> Int) -> [a] -> (a -> [(a, Int)]) -> HashMap.HashMap a Int
astarDists heuristic start neigh = go HashSet.empty (HashMap.fromList $ map (, 0) start) (PQ.fromList $ map (0, ) start)
  where
    go seen dists queue = case PQ.minViewWithKey queue of
      Just ((curDist, cur), rest) ->
        if | HashSet.member cur seen -> go seen dists rest
           | otherwise -> let neighs = neigh cur
                              newSeen = HashSet.insert cur seen
                              f (other, toOther) = let newDist = curDist + toOther
                                in if all (> newDist) $ maybeToList (dists HashMap.!? other)
                                      then Just (other, newDist)
                                      else Nothing
                              forAdd = mapMaybe f neighs
                              newDists = foldr (\(x, d) m -> HashMap.insert x d m) dists forAdd
                              newQ = foldr (\(x, d) q -> PQ.insert (d + heuristic x) x q) rest forAdd
                           in go newSeen newDists newQ
      Nothing -> dists

neighs mat x = map (, 1) $ filter ((/= '#') . (mat Arr.!)) $ susedi4 (Just $ Arr.bounds mat) x

distsFrom :: CharMatrix -> V2 Int -> HashMap.HashMap (V2 Int) Int
distsFrom mat src = astarDists (const 0) [src] (neighs mat)

solve n mat = listCount (\d -> noCheatsDist - d >= 100) $ mapMaybe distCheat cheats
  where
    mapaS = distsFrom mat (p0 'S')
    mapaE = distsFrom mat (p0 'E')
    p0 ch = fst $ fromJust $ find ((== ch) . snd) $ Arr.assocs mat
    noCheatsDist = mapaS HashMap.! (p0 'E')
    cheats = concatMap (\p -> map (\(y, d) -> (p, y, d)) $ filter ((/= '#') . (arrLookupWithDefault '#' mat) . fst) $ hood p)
                    $ map fst $ filter ((/= '#') . snd) $ Arr.assocs mat
    hood p = [ (p + V2 i j, abs i + abs j)
               | i <- [-n .. n]
               , let s = n - abs (i)
               , j <- [ -s .. s ] ]
    distCheat (x1, x2, d) = do
      d1 <- mapaS HashMap.!? x1
      d2 <- mapaE HashMap.!? x2
      return $ d1 + d + d2

main20 :: IO ()
main20 = do
    input <- parseMatrix <$> readFile "res/input20"
    print $ solve 2 input
    print $ solve 20 input
