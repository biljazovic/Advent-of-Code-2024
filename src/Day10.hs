module Day10 (main10) where

import Util
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array ( Array )
import qualified Data.Array as Arr
import qualified Data.Sequence as Seq

solveA mat = sum $ map scoreOf $ map fst $ filter ((== '0') . snd) $ Arr.assocs mat
  where
    scoreOf x = listCount ((== '9') . (mat Arr.!)) $ Set.toList $ seenOf x

    seenOf x = bfs (Set.singleton x) (Seq.singleton x)

    neighs p = filter ((== (succ (mat Arr.! p))) . (mat Arr.!)) $ susedi4 (Just (Arr.bounds mat)) p

    bfs seen queue = case queue of
      (Seq.viewl -> cur Seq.:< rest) ->
            let (newSeen, newQueue) = foldr g (seen, rest) (neighs cur)
                g neigh (seen', queue') =
                  if Set.member neigh seen'
                    then (seen', queue')
                    else (Set.insert neigh seen', queue' Seq.|> neigh)
             in bfs newSeen newQueue
      _ -> seen

solveB mat = sum $ map scoreOf $ map fst $ filter ((== '0') . snd) $ Arr.assocs mat
  where
    scoreOf x = sum . map snd . filter ((== '9') . (mat Arr.!) . fst) . Map.assocs $ seenOf x

    seenOf x = bfs (Map.singleton x 1) (Seq.singleton x)

    neighs p = filter ((== (succ (mat Arr.! p))) . (mat Arr.!)) $ susedi4 (Just (Arr.bounds mat)) p

    bfs seen queue = case queue of
      (Seq.viewl -> cur Seq.:< rest) ->
            let num = seen Map.! cur
                (newSeen, newQueue) = foldr g (seen, rest) (neighs cur)
                g neigh (seen', queue') =
                  if Map.member neigh seen'
                    then (Map.adjust (+ num) neigh seen', queue')
                    else (Map.insert neigh num seen', queue' Seq.|> neigh)
             in bfs newSeen newQueue
      _ -> seen

main10 :: IO ()
main10 = do
    input <- parseMatrix <$> readFile "res/input10"
    print $ solveA input
    print $ solveB input
