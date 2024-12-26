module Day15 (main15) where

import Util
import Data.Maybe
import qualified Data.Array.IArray as Arr
import Data.Containers.ListUtils (nubOrd, nubOrdOn)

toDir :: Char -> V2 Int
toDir = \case
  '>' -> V2 0 1
  '<' -> V2 0 (-1)
  '^' -> V2 (-1) 0
  'v' -> V2 1 0

gps :: V2 Int -> Int
gps (V2 x y) = x * 100 + y

moveA (mat, x) dir = if (mat Arr.! x1) == '#' then (mat, x) else (mat', x + dir)
  where
    os = takeWhile ((== 'O') . (mat Arr.!)) $ tail $ iterate (+ dir) x
    x1 = if null os then x + dir else (last os) + dir
    mat' = mat Arr.// ([ (x, '.'), (x+dir, '@') ] ++ [ (x1, 'O')  | not (null os) ])

solveA mat moves = mat' & Arr.assocs & filter ((=='O') . snd) & map (fst >>> gps) & sum
  where
    (mat', _) = foldl moveA (mat, x0) $ map toDir moves
    x0 = fst $ fromJust $ find ((=='@') . snd) $ Arr.assocs mat

widen :: CharMatrix -> CharMatrix
widen mat = Arr.array (V2 0 0, V2 n (2*m+1)) accs
  where
    (V2 0 0, V2 n m) = Arr.bounds mat
    accs = Arr.assocs mat & concatMap (\(V2 x y, ch) -> zip [V2 x (2*y), V2 x (2*y+1)] (widenChar ch))
    widenChar = \case
      '.' -> ".."
      '#' -> "##"
      'O' -> "[]"
      '@' -> "@."

solveB (widen -> mat) moves = mat' & Arr.assocs
                                   & filter ((=='[') . snd)
                                   & map (fst >>> gps)
                                   & sum
    where
      (mat', _) = foldl moveB (mat, x0) $ map toDir moves
      x0 = fst $ fromJust $ find ((=='@') . snd) $ Arr.assocs mat

moveB (mat, x) dir = case expand [x] of
                       Nothing -> (mat, x)
                       Just pss -> (mat', x+dir)
                        where
                          mat' = mat Arr.// nubOrdOn fst changes
                          f p = let ch = mat Arr.! p
                                 in [ (p, '.'), (p + dir, ch) ]
                          changes = concatMap f pss
  where
    expandOne (p, v) dir@(V2 dx dy) =
      if dx == 0
         then [p]
         else case v of
               ']' -> [p - V2 0 1, p]
               '[' -> [p, p + V2 0 1]

    expand :: [V2 Int] -> Maybe [V2 Int]
    expand ps = let ps' = nubOrd $ map (+dir) ps
                    pvs' = map (\x -> (x, mat Arr.! x)) ps'
                    ps'' = nubOrd
                            $ concatMap (\(p, v) -> expandOne (p, v) dir)
                            $ filter ((/= '.') . snd) pvs'
                 in if | any ((== '#') . snd) pvs' -> Nothing
                       | null ps'' -> Just ps
                       | otherwise -> fmap (ps ++) $ expand ps''


main15 :: IO ()
main15 = do
    [parseMatrix -> mat, concat . lines -> moves] <- splitOn "\n\n" <$> readFile "res/input15"
    print $ solveA mat moves
    print $ solveB mat moves
