module Day05 (main05) where

import Util
import Data.Maybe (isNothing)
import Control.Monad (guard)

import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Graph.Inductive.Graph (mkUGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)

matches rules lst = all f rules
  where
    f [a, b] = let x = do
                    i <- elemIndex a lst
                    j <- elemIndex b lst
                    guard (i > j)
                in isNothing x

solveA rules lsts = sum
  $ map (\l -> l !! (length l `div` 2))
  $ filter (matches rules) lsts

topo rules' lst = topsort (mkUGraph xs edges :: Gr () ())
  where
    rules = filter (all (`elem` lst)) rules'
    edges = map (\[x, y] -> (x, y)) rules
    xs = mkUniq $ concat rules

solveB rules lsts = sum
  $ map (\l -> l !! (length l `div` 2))
  $ map (topo rules)
  $ filter (not . matches rules) lsts

main05 :: IO ()
main05 = do
    [rules', lsts'] <- map lines . splitOn "\n\n" <$> readFile "res/input05"
    let rules = map (map read . splitOn "|") rules' :: [[Int]]
    let lsts = map (map read . splitOn ",") lsts' :: [[Int]]
    print $ solveA rules lsts
    print $ solveB rules lsts
