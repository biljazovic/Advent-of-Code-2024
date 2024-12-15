module Day14 (main14) where

import Util
import Data.Maybe 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array ( Array )
import qualified Data.Array as Arr
import Text.Scanf
import Control.Monad (forM_)

type Robot = (V2 Int, V2 Int)

(wx, wy) = (101, 103)

parseLine :: String -> Robot
parseLine str = (V2 px py, V2 vx vy)
  where
    Just (px :+ py :+ vx :+ vy :+ ()) = scanf [fmt|p=%d,%d v=%d,%d|] str

step :: Robot -> Robot
step (p@(V2 x y), dir@(V2 dx dy)) = (V2 x' y', dir)
  where
    x' = (x + dx + wx) `mod` wx
    y' = (y + dy + wy) `mod` wy

solveA rs = product $ map length $ group $ sort 
                $ map (\(x, y) -> (fst x > 0, fst y > 0))
                $ filter (\(x, y) -> x /= disallowed && y /= disallowed) $ map (f . fst) rs'
  where
    rs' = (iterate (map step) rs) !! 100
    f (V2 x y) = (x `divMod` wx2, y `divMod` wy2)
    (wx2, wy2) = (wx `div` 2, wy `div` 2)
    disallowed = (1, 0)

possible :: [Robot] -> Bool
possible rs = maximum (sums rs) > 20

sums rs = map length $ group $ sort $ map f rs
f (V2 x y, _) = y

toColor = \case
  1 -> PixelRGB8 0 0 0
  0 -> PixelRGB8 255 255 255

drawRobots :: Int -> [Robot] -> IO ()
drawRobots ind rs = generateBlackAndWhiteImage ("res/output" ++ show ind ++ ".bmp") 0 mapa toColor
  where
    mapa = Map.fromList $ zip (map fst rs) (repeat 1)

main14 :: IO ()
main14 = do
    input <- map parseLine . lines <$> readFile "res/input14"
    print $ solveA input
    forM_ (take 100 $ filter (fst >>> possible) $ zip (iterate (map step) input) [0..])
        $ (\(rs, ind) -> drawRobots ind rs)
