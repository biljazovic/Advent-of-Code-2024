module Day16 (main16) where

import Util
import Data.Maybe
import Data.Array ( Array )
import qualified Data.Array as Arr
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)

neighs mat (p, dir) = [ ((p, turnR dir), 1000), ((p, turnL dir), 1000) ]
                  ++ [ ((p + dir, dir), 1) | mat Arr.! (p+dir) /= '#' ]

solveA mat = dijkstra
    [ (p0, V2 0 1) ]
    ((== 'E') . (mat Arr.!) . fst)
    (neighs mat)
  where
    p0 = fst $ fromJust $ find ((== 'S') . snd) $ Arr.assocs mat

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

distsFrom :: CharMatrix -> [(V2 Int, V2 Int)] -> HashMap.HashMap (V2 Int, V2 Int) Int
distsFrom mat src = mapa'
  where
    mapa' = astarDists (const 0) src (neighs mat)

dirs = [ V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0 ]

solveB mat = listCount isOnShortestPath $ Arr.indices mat
  where
    isOnShortestPath p = mat Arr.! p /= '#' && f p == dist
    f p = minimum $ [ mapaS HashMap.! (p, dir) + mapaE HashMap.! (p, turnR (turnR (dir))) | dir <- dirs ]
    Just dist = solveA mat
    mapaS = distsFrom mat [ (p0 'S', V2 0 1) ]
    mapaE = distsFrom mat [ (p0 'E', dir) | dir <- dirs ]
    p0 ch = fst $ fromJust $ find ((== ch) . snd) $ Arr.assocs mat

main16 :: IO ()
main16 = do
    input <- parseMatrix <$> readFile "res/input16"
    print $ solveA input
    print $ solveB input
