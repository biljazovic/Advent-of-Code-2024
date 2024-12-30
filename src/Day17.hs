module Day17 (main17) where

import Util
import Data.Maybe
import Data.Digits (unDigits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits

combo :: [Integer] -> Int -> Integer
combo regs x
  | x < 4 = toInteger x
  | otherwise = regs !! (x - 4)

adv oper (p, regs@[a, b, c], outs) = (p + 2, [ a `div` (2 ^ (combo regs oper)), b, c ], outs)
bxl oper (p, regs@[a, b, c], outs) = (p + 2, [ a, b `xor` toInteger oper, c ], outs)
bst oper (p, regs@[a, b, c], outs) = (p + 2, [ a, (combo regs oper) `mod` 8, c ], outs)
jnz oper (p, regs@[a, b, c], outs) = (if a == 0 then p + 2 else oper, regs, outs)
bxc _    (p, regs@[a, b, c], outs) = (p + 2, [ a, b `xor` c, c ], outs)
out oper (p, regs@[a, b, c], outs) = (p + 2, regs, ((combo regs oper) `mod` 8) : outs)
bdv oper (p, regs@[a, b, c], outs) = (p + 2, [ a, a `div` (2 ^ (combo regs oper)), c ], outs)
cdv oper (p, regs@[a, b, c], outs) = (p + 2, [ a, b, a `div` (2 ^ (combo regs oper)) ], outs)

ops = [ adv, bxl, bst, jnz, bxc, out, bdv, cdv ] :: [ Int -> (Int, [Integer], [Integer]) -> (Int, [Integer], [Integer]) ]

runProg n prog args@(p, regs, outs)
  | p >= length prog = reverse $ outs
  | otherwise = let op = ops !! (prog !! p)
                    oper = prog !! (p + 1)
                 in runProg (n+1) prog (op oper args)

search mapa startDigs ls = concat [ go startDig ls | startDig <- startDigs ]
  where
    go d [] = [d]
    go d (x : xs) =
      let chs = filter ((== (take 3 d)) . drop 1) $ concat $ maybeToList $ mapa Map.!? x
       in concat [ go (head ch : d) xs | ch <- chs ]

main17 :: IO ()
main17 = do
    [map ((read :: String -> Integer) . (!! 2) . splitOn " ") . lines -> regs, map (read :: String -> Integer) . splitOn "," . (!! 1) . splitOn " " . head . lines -> prog] <- splitOn "\n\n" <$> readFile "res/input17"
    let f a = runProg 0 (map fromInteger prog) (0, [a, 0, 0], [])
    putStrLn $ concat $ intersperse "," $ map show $ f (regs !! 0)
    let mapa1 = Map.fromListWith (++) $ [ (f $ unDigits 8 [ i3, i2, i1, i0 ], [[i0, i1, i2, i3]]) | i0 <- [0..7], i1 <- [0..7], i2 <- [0..7], i3 <- [1..7] ]
        mapa = Map.mapKeysWith (++) head mapa1
        mydigs = search mapa (mapa1 Map.! (reverse $ take 4 $ reverse prog)) (drop 4 $ reverse prog)
    print $ minimum $ map (unDigits 8 . reverse) mydigs
