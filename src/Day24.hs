module Day24 (main24) where

import Util
import Data.Maybe
import Data.Digits (unDigits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Graph.Inductive.Graph (mkUGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Containers.ListUtils (nubOrd)
import Data.Bits

parseInits :: String -> [(String, Int)]
parseInits input = map f $ lines input
  where
    f str = let [str1, str2] = splitOn ": " str
             in (str1, read str2)

parseInstrs :: String -> [(String, (String, String), String)]
parseInstrs input = map f $ lines input
  where
    f str = let [splitOn " " -> [oper1, op, oper2], res] = splitOn " -> " str
             in (op, (oper1, oper2), res)

makeGraph :: [(String, (String, String), String)] -> (Gr () (), Int, Map Int String)
makeGraph instrs = (mkUGraph [0..n] edges, n, indexMapR)
  where
    indexMap = Map.fromList $ zipWith (,) vertices [0..]
    indexMapR = Map.fromList $ map swap $ Map.assocs indexMap
    n = length vertices - 1
    vertices = nubOrd $ concatMap (\(op, (oper1, oper2), res) -> [oper1, oper2, res]) instrs
    edges' = concatMap (\(op, (oper1, oper2), res) -> [(oper1, res), (oper2, res)]) instrs
    edges = indexize edges'
    indexize = map (\(a, b) -> (indexMap Map.! a, indexMap Map.! b))

doOp op oper1 oper2 = case op of
  "AND" -> oper1 .&. oper2
  "OR"  -> oper1 .|. oper2
  "XOR" -> oper1 `xor` oper2

solveA inits instrs = unDigits 2 . reverse . map snd . sortOn (fst >>> readNum) . filter ((== 'z') . head . fst) $ Map.assocs mapa
  where
    readNum str = read (drop 1 str) :: Int
    operMap = Map.fromList $ map (\(op, (oper1, oper2), res) -> (res, (op, (oper1, oper2)))) instrs
    (gr, n, indexMapR) = makeGraph instrs
    mapa = foldl' f (Map.fromList inits) $ topsort gr
    f :: Map String Int -> Int -> Map String Int
    f mapa' v = if Map.member (indexMapR Map.! v) mapa'
                   then mapa'
                   else let (op, (oper1, oper2)) = operMap Map.! (indexMapR Map.! v)
                            value = doOp op (mapa' Map.! oper1) (mapa' Map.! oper2)
                         in Map.insert (indexMapR Map.! v) value mapa'

solveB inits instrs = badzs ++ filter badx xs
  where
    badzs = filter ((\(op, _) -> op /= "XOR") . (operMap Map.!)) zs
    badx x = let Just r1 = findXOR x
              in (r1 /= "z00") && (isNothing $ findXOR r1)
    findXOR str = do
      (_, _, res) <- find (\(op, (oper1, oper2), _) -> op == "XOR" && (oper1 == str || oper2 == str)) instrs
      return res
    n = (length inits `div` 2) - 1
    f x = if x < 10 then "0" ++ show x
                    else show x
    g ch = [ ch : f i | i <- [0..n] ]
    [xs, ys, zs] = map g "xyz"
    operMap = Map.fromList $ map (\(op, (oper1, oper2), res) -> (res, (op, (oper1, oper2)))) instrs

main24 :: IO ()
main24 = do
    [parseInits -> inits, parseInstrs -> instrs] <- splitOn "\n\n" <$> readFile "res/input24"
    print $ solveA inits instrs
    print $ solveB inits instrs
