module Day12 (main12) where

import Util
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Array.IArray as Arr

dirs = [ V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1) ]

-- TODO ugly
flood :: (CharMatrix -> V2 Int -> V2 Int -> Bool) -> CharMatrix -> Set (V2 Int) -> V2 Int -> ((Int, Int), Set (V2 Int))
flood periTest mat vis p = dfs vis p
  where
    ch = mat Arr.! p
    dfs vis' p' = let
      p's = susedi4 Nothing p'
      good p'' = arrLookupWithDefault '\0' mat p'' == ch
      p''s = filter good p's
      ((area0, peri0), vis0) = foldr f ((0, 0), Set.insert p' vis') p''s
      f p'' ((area1, peri1), vis'') = let
              ((area2, peri2), vis''') = dfs vis'' p''
                                  in ((area1 + area2, peri1 + peri2), vis''')
         in if Set.member p' vis'
               then ((0, 0), vis')
               else ((area0 + 1, peri0 + listCount (periTest mat p') dirs), vis0)

solve periTest mat = fst $ foldr f (0, Set.empty) $ Arr.assocs mat
  where
    f (p, ch) (cum, vis) = let
      ((area1, peri1), vis1) = flood periTest mat vis p
                in (cum + area1 * peri1, vis1)

isPeri :: CharMatrix -> V2 Int -> V2 Int -> Bool
isPeri mat p dir = arrLookupWithDefault '\0' mat (p + dir) /= ch
  where
    ch = mat Arr.! p

isSide :: CharMatrix -> V2 Int -> V2 Int -> Bool
isSide mat p dir = arrLookupWithDefault '\0' mat (p + dir) /= ch
                    && (arrLookupWithDefault '\0' mat p' /= ch
                          || arrLookupWithDefault '\0' mat (p' + dir) == ch)
  where
    ch = mat Arr.! p
    p' = p + turnR dir

main12 :: IO ()
main12 = do
    input <- parseMatrix <$> readFile "res/input12"
    print $ solve isPeri input
    print $ solve isSide input
