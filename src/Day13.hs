module Day13 (main13) where

import Util
import Data.Maybe (mapMaybe)
import qualified Data.Array.IArray as Arr
import Control.Exception (assert)
import Text.Scanf

parseButton :: String -> (Integer, Integer)
parseButton str = (toInteger a, toInteger b)
  where
    Just (x :+ a :+ b :+ ()) = scanf [fmt|Button %c: X+%d, Y+%d|] str


parsePrize :: String -> (Integer, Integer)
parsePrize str = (toInteger a, toInteger b)
  where
    Just (a :+ b :+ ()) = scanf [fmt|Prize: X=%d, Y=%d|] str

type Entry = ((Integer, Integer), (Integer, Integer), (Integer, Integer))

parseEntry :: String -> Entry
parseEntry (lines -> [l1, l2, l3]) = (parseButton l1, parseButton l2, parsePrize l3)

solveEntry :: Entry -> Maybe (Integer, Integer)
solveEntry ((xa, ya), (xb, yb), (xf, yf)) = assert (det /= 0) sol
  where
    det = xa * yb - xb * ya
    sol = if dn == 0 && dm == 0
             then Just (n, m)
             else Nothing
    n' = yb * xf - xb * yf
    m' = -ya * xf + xa * yf
    (n, dn) = n' `divMod` det
    (m, dm) = m' `divMod` det

solveA = sum . map score . filter (\(n, m) -> Arr.inRange (0, 100) n && Arr.inRange (0, 100) m) . mapMaybe solveEntry

score (n, m) = 3 * n + m

solveB = sum . map score . mapMaybe solveEntry . map (\(b1, b2, (xf, yf)) -> (b1, b2, (xf + add, yf + add)))
  where
    add = 10000000000000

main13 :: IO ()
main13 = do
    input <- map parseEntry . splitOn "\n\n" <$> readFile "res/input13"
    print $ solveA input
    print $ solveB input
