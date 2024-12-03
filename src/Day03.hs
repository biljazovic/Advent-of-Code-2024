module Day03 (main03) where

import Util
import Text.ParserCombinators.ReadP
import Control.Monad (guard)

parseMul :: ReadP (Int, Int)
parseMul = do
  a <- string "mul(" *> parseInt <* char ','
  b <- parseInt <* char ')'
  guard (a < 1000 && b < 1000)
  return (a, b)

solveA = sum . map ((\(a, b) -> a * b) . fst) . readP_to_S (many get *> parseMul)

data BD = Do | Dont | Mul Int Int | Noop
  deriving Show

parseB :: ReadP [BD]
parseB = many1 $
  (string "do()" $> Do)               <++
  (string "don't()" $> Dont)          <++
  ((\(a, b) -> Mul a b) <$> parseMul) <++
  (get $> Noop)

solveB = snd . foldl f (True, 0) . fst . head . readP_to_S (parseB <* eof)
  where
    f (cur, s) = \case
      Do -> (True, s)
      Dont -> (False, s)
      Mul a b -> (cur, if cur then s + a * b else s)
      Noop -> (cur, s)

main03 :: IO ()
main03 = do
    input <- readFile "res/input03"
    print $ solveA input
    print $ solveB input
