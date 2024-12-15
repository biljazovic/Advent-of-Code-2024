module Day11 (main11) where

import Util
import Data.Digits (digits, unDigits)
import Data.Map (Map)
import qualified Data.Map as Map

blinkOne :: Integer -> [Integer]
blinkOne x = if
                | x == 0 -> [1]
                | len `mod` 2 == 0 -> [ unDigits 10 (take (len `div` 2) ds), unDigits 10 (drop (len `div` 2) ds) ]
                | otherwise -> [x * 2024]
                  where
                    ds = digits 10 x
                    len = length ds

blink :: Map Integer Integer -> Map Integer Integer
blink = Map.assocs >>> concatMap f >>> Map.fromListWith (+)
  where
    f (x, freq) = map (, freq) $ blinkOne x

solveA = iterate blink >>> (!! 25) >>> Map.elems >>> sum
solveB = iterate blink >>> (!! 75) >>> Map.elems >>> sum

main11 :: IO ()
main11 = do
    input <- Map.fromListWith (+) . map (\x -> (x, 1)) . map read . splitOn " " <$> readFile "res/input11"
    print $ solveA input
    print $ solveB input
