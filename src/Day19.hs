module Day19 (main19) where

import Util
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import Data.Function.Memoize

ways patterns designs = map good designs
  where
    m = maximum $ map length patterns
    ps = Set.fromList $ patterns
    good = memoFix $ \good' design ->
      let f (pre, rest) = if Set.member pre ps
                             then good' rest
                             else 0
       in
        if null design
           then 1
           else sum $ map f [ (take i design, drop i design) | i <- [1 .. min m (length design) ] ]

main19 :: IO ()
main19 = do
    [splitOn ", " . head . lines -> patterns, lines -> designs] <- splitOn "\n\n" <$> readFile "res/input19"
    let ws = ways patterns designs
    print $ listCount (> 0) ws
    print $ sum ws
