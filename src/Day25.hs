module Day25 (main25) where

import Util
import Data.Maybe

data T = Key [Int] | Lock [Int] deriving (Show, Eq)

parse :: String -> T
parse str = if head (head chs) == '#'
               then Lock (toxs chs)
               else Key (toxs (map reverse chs))
  where
    chs = transpose (lines str)
    toxs = map ((+(-1)) . length . takeWhile (== '#'))

fits (Key xs) (Lock ys) = all (<= 5) (zipWith (+) xs ys)

solveA input = listCount id [ fits k l | k <- keys, l <- locks ]
  where
    (keys, locks) = foldl' (\(keys', locks') k -> case k of
                                Key _ -> (k:keys', locks')
                                _ -> (keys', k:locks')) ([], []) input

main25 :: IO ()
main25 = do
    input <- map parse . splitOn "\n\n" <$> readFile "res/input25"
    print $ solveA input
