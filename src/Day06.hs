module Day06 (main06) where

import Util
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import qualified Data.Array as Arr
import Control.Parallel.Strategies (parMap, rdeepseq)

step :: CharMatrix -> (V2 Int, V2 Int) -> Maybe (V2 Int, V2 Int)
step mat (x, dir) = 
  case arrLookup (x + dir) mat of
    Nothing -> Nothing
    Just '.' -> Just ((x + dir), dir)
    Just '^' -> Just ((x + dir), dir)
    Just '#' -> Just (x, (turnR dir))

solveA mat = length $ go (Set.singleton x0) x0 dir0
  where
    x0 = fst $ fromJust $ find ((== '^') . snd) $ Arr.assocs mat
    dir0 = V2 (-1) 0
    go vis x dir =
      case step mat (x, dir) of
        Nothing -> vis
        Just (x', dir') -> go (Set.insert x' vis) x' dir'

solveB mat = listCount (== True) $ parMap rdeepseq isLoop kands
  where
    x0 = fst $ fromJust $ find ((== '^') . snd) $ Arr.assocs mat
    kands = map fst $ filter ((== '.') . snd) $ Arr.assocs mat
    isLoop kand = let mat' = mat Arr.// [(kand, '#')]
                   in isJust $ findCycle (step mat') (x0, V2 (-1) 0)

main06 :: IO ()
main06 = do
    mat <- parseMatrix <$> readFile "res/input06"
    print $ solveA mat
    print $ solveB mat
