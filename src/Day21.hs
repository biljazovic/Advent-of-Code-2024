module Day21 (main21) where

import Util
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Array.IArray as Arr
import Data.Char (isDigit)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

digitBoard = parseMatrix $ unlines ["789", "456", "123", "\0" ++ "0A"]
dirBoard = parseMatrix $ unlines ["\0" ++ "^A", "<v>"]

charP :: CharMatrix -> Char -> V2 Int
charP mat ch = fst $ fromJust $ find ((== ch) . snd) $ Arr.assocs mat

char2Dir = \case
  '>' -> V2 0 1
  '<' -> V2 0 (-1)
  'v' -> V2 1 0
  '^' -> V2 (-1) 0
dirs = "><^v"

dijkstraPath :: (Eq a, Hashable a) =>  [a] -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe [a]
dijkstraPath start goal neighF = go HashSet.empty (HashMap.fromList $ map (\s -> (s, (0, [s]))) start) (PQ.fromList $ map (0, ) start)
  where
    go seen dists queue = do
      ((curDist, cur), rest) <- PQ.minViewWithKey queue
      (_, curPath) <- dists HashMap.!? cur
      if | HashSet.member cur seen -> go seen dists rest
         | goal cur -> Just curPath
         | otherwise -> let nexts = neighF cur
                            newSeen = HashSet.insert cur seen
                            f (other, toOther) = let newDist = curDist + toOther
                              in if all ((> newDist) . fst) $ maybeToList (dists HashMap.!? other)
                                    then Just (other, newDist)
                                    else Nothing
                            forAdd = mapMaybe f nexts
                            newDists = foldr (\(x, d) m -> HashMap.insert x (d, curPath ++ [x]) m) dists forAdd
                            newQ = foldr (\(x, d) q -> PQ.insert d x q) rest forAdd
                         in go newSeen newDists newQ

---------------- SOLVE NAIVELY WITH N ROBOTS BETWEEN -- TOO SLOW FOR N=25 ---------------------------

directionsFromDijkstraPath steps path =
  (++ "A") . concat . map (drop 1) . group
    . map (!! (steps-1))
    . filter ((> (steps-2)) . length . takeWhile (== 'A'))
    $ path

solveDirExact :: Int -> Char -> Char -> String
solveDirExact steps from to = directionsFromDijkstraPath steps path
  where
    Just path = dijkstraPath [(replicate steps 'A') ++ [from]] goal (\x -> map (, 1) $ neighsDir x)
    goal str = str == (replicate steps 'A' ++ [to])

neighsDir ds = [ dir2' : tail ds | dir2' <- neighsBoard dirBoard (head ds) ]
    ++ others
  where
    as = takeWhile (== 'A') ds
    notas = dropWhile (== 'A') ds
    others = if
                | length notas <= 1 -> []
                | otherwise -> let (dir1 : dir2 : notas') = notas
                                in [ as ++ [dir1, dir2'] ++ notas' | Just dir2' <- [ move dirBoard dir2 dir1 ] ]

solveDigExact :: Int -> Char -> Char -> String
solveDigExact steps from to = directionsFromDijkstraPath steps path
  where
    Just path = dijkstraPath [(replicate steps 'A') ++ [from]] goal (\x -> map (, 1) $ neighsDig x)
    goal str = (str == (replicate steps 'A') ++ [to])

neighsDig ds = [ dir2' : tail ds | dir2' <- neighsBoard dirBoard (head ds) ]
    ++ others
  where
    as = takeWhile (== 'A') ds
    notas = dropWhile (== 'A') ds
    others = if
                | length notas <= 1 -> []
                | length notas == 2 -> let [ dir2, ch ] = notas
                                        in [ as ++ [dir2, ch'] | Just ch' <- [move digitBoard ch dir2] ]
                | otherwise -> let (dir1 : dir2 : notas') = notas
                                in [ as ++ [dir1, dir2'] ++ notas' | Just dir2' <- [ move dirBoard dir2 dir1 ] ]

neighsBoard board ch = catMaybes [ move board ch dir | dir <- dirs ]
move board ch dir = case arrLookupWithDefault '\0' board (charP board ch + char2Dir dir) of
                      '\0' -> Nothing
                      ch' -> Just ch'

------------------------------------ 5 IS ENOUGH TO GET OPTIMAL STEPS -----------------------
enoughSteps = 5

solveWord steps dirSolsCache str =
  calcLength $ iterate dirMoves (Map.fromListWith (+) $ map (, 1) $ pairs s0) !! steps
  where
    digMoves str = fst $ foldl g ("", 'A') str
    g (path, ch1) ch = (path ++ (solveDigExact enoughSteps ch1 ch), ch)
    s0 = digMoves str
    dirMoves mapa = Map.fromListWith (+)
                      $ concatMap (\((ch, ch1), cnt) ->
                          let path = dirSolsCache Map.! (ch, ch1)
                           in map (, cnt) $ pairs path)
                      $ Map.assocs mapa
    pairs str = [('A', head str)] ++ zip str (drop 1 str)
    calcLength mapa = sum (Map.elems mapa)

solve steps dirSolsCache input = sum . map score $ input
  where
    score str = ((read (takeWhile isDigit str)) :: Integer) * solveWord steps dirSolsCache str

main21 :: IO ()
main21 = do
    input <- lines <$> readFile "res/input21"
    let dirs = filter (/= '\0') . nub . Arr.elems $ dirBoard
        dirPairs = [ (a, b) | a <- dirs, b <- dirs ]
        dirSolsCache = Map.fromList $ map (\p@(ch, ch1) -> (p, solveDirExact enoughSteps ch ch1)) dirPairs
    print $ solve 2 dirSolsCache input
    print $ solve 25 dirSolsCache input
