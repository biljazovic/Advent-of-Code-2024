module Day01 (main01) where

import Util
import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.IntMap.Strict as IntMap

solveA input = sum $ zipWith (\a b -> abs (a - b)) (inputS !! 0) (inputS !! 1)
  where
    inputS = map sort $ transpose input

solveB input = sum $ map (\x -> x * (IntMap.findWithDefault 0 x freqs)) (inputS !! 0)
  where
    inputS = map sort $ transpose input
    freqs = freqIntMap (inputS !! 1)

-- $> main01
main01 :: IO ()
main01 = do
  input <- map (map read . words) . lines <$> readFile "res/input01" :: IO [[Int]]
  print $ solveA input
  print $ solveB input
