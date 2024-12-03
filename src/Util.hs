module Util
  ( module Linear.V2,
    module Data.Void,
    module Data.List,
    module Data.List.Split,
    module Control.Arrow,
    module Data.Function,
    module Data.Functor,
    module Codec.Picture,
    parseMatrix,
    CharMatrix,
    sepBy1_,
    emptyLine,
    generateBlackAndWhiteImage,
    generateGraph,
    susedi4,
    susedi8,
    listToArray,
    listCount,
    inBounds,
    scale,
    argmin,
    argmax,
    parseInt,
    newline,
    freqIntMap,
    freqMap,
    susedi,
    matrixToString,
    iterateUntil,
    traceVar,
    arrLookupWithDefault,
    mkUniq,
    showAnimation,
    dijkstra,
    genericBfs,
    astar,
    swap,
    groupByn,
    space,
    trim,
    whenM,
    makePairs,
    iterateWithCycle
  ) where

import Codec.Picture
    ( saveBmpImage, generateImage, DynamicImage(ImageRGB8), PixelRGB8(..) )
import Control.Lens ( (^.) )
import Control.Monad (guard, forM_)
import Data.Array ( Ix(inRange), Array, array, listArray )
import qualified Data.Array as Arr
import Data.Char (isSpace)
import Data.Graph.Inductive.Graph ( Graph(mkGraph) )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust, maybeToList)
import qualified Data.Set as Set
import Data.Void (Void)
import Linear.V2
import Text.Megaparsec (MonadParsec, try, many)
import Text.ParserCombinators.ReadP (ReadP, option, char, string, many1)
import qualified Text.ParserCombinators.ReadP as ReadP
import Data.Functor (($>))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Control.Arrow ((>>>))
import Data.Function ((&))
import Debug.Trace (trace)

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import System.Console.ANSI
import Control.Concurrent (threadDelay)
import qualified Data.Sequence as Seq

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

space :: ReadP ()
space = many1 (char ' ') $> ()

newline :: ReadP ()
newline = ReadP.char '\n' $> ()

parseInt :: ReadP Int
parseInt = do
  read <$> ((++) <$> option "" (string "-") <*> ReadP.munch1 (`elem` ['0'..'9']))

type CharMatrix = Array (V2 Int) Char

matrixToString :: CharMatrix -> String
matrixToString mat = unlines [ [ mat Arr.! V2 i j | j <- [y1..y2] ] | i <- [x1..x2] ]
  where
    (V2 x1 y1, V2 x2 y2) = Arr.bounds mat

parseMatrix :: String -> CharMatrix
parseMatrix strToParse =
  let strs = filter (not . emptyLine) $ lines strToParse
      strs' = map fillWithSpaces strs
      fillWithSpaces str = str ++ replicate (m - length str) ' '
      n = length strs
      m = maximum $ map length strs
      lst = concat $ [[(V2 i j, ch) | (ch, j) <- zip str [0 ..]] | (str, i) <- zip strs' [0 ..]]
   in array (V2 0 0, V2 (n -1) (m -1)) lst

sepBy1_ :: MonadParsec e s m => m a -> m sep -> m [a]
sepBy1_ p sep = (:) <$> try p <*> many (try (sep *> p))


dijkstra :: (Eq a, Hashable a) => [a] -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe Int
dijkstra = astar (const 0)

astar :: (Eq a, Hashable a) =>  (a -> Int) -> [a] -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe Int
astar heuristic start goal neigh = go HashSet.empty (HashMap.fromList $ map (, 0) start) (PQ.fromList $ map (0, ) start)
  where
    go seen dists queue = do
      ((curDist, cur), rest) <- PQ.minViewWithKey queue
      if | HashSet.member cur seen -> go seen dists rest
         | goal cur -> Just curDist
         | otherwise -> let neighs = neigh cur
                            newSeen = HashSet.insert cur seen
                            f (other, toOther) = let newDist = curDist + toOther
                              in if all (> newDist) $ maybeToList (dists HashMap.!? other)
                                    then Just (other, newDist)
                                    else Nothing
                            forAdd = mapMaybe f neighs
                            newDists = foldr (\(x, d) m -> HashMap.insert x d m) dists forAdd
                            newQ = foldr (\(x, d) q -> PQ.insert (d + heuristic x) x q) rest forAdd
                         in go newSeen newDists newQ

genericBfs ::
  Ord a =>
  (a -> Bool) -> -- goal
  (a -> [a]) -> -- neighbors
  [a] -> -- start
  Maybe Int -- distance from start to goal
genericBfs goal neighs start = bfs (Set.fromList start) (Seq.fromList $ map (, 0) start)
  where
    bfs seen queue = case queue of
      (Seq.viewl -> (cur, dist) Seq.:< rest) ->
        if goal cur
          then Just dist
          else
            let (newSeen, newQueue) = foldr g (seen, rest) (neighs cur)
                g neigh (seen', queue') =
                  if Set.member neigh seen'
                    then (seen', queue')
                    else (Set.insert neigh seen', queue' Seq.|> (neigh, dist + 1))
             in bfs newSeen newQueue
      _ -> Nothing

generateBlackAndWhiteImage :: String -> a -> Map (V2 Int) a -> (a -> PixelRGB8) -> IO ()
generateBlackAndWhiteImage filename defaultValue mapa toColor = saveBmpImage filename image
  where
    image = ImageRGB8 $ generateImage f (maxX - minX + 1) (maxY - minY + 1)
    f x y = toColor $ Map.findWithDefault defaultValue (V2 (x + minX) (y + minY)) mapa
    coords = Map.keysSet mapa
    coordsx = Set.map (^. _x) coords
    coordsy = Set.map (^. _y) coords
    [minX, maxX] = map ($ coordsx) [Set.findMin, Set.findMax]
    [minY, maxY] = map ($ coordsy) [Set.findMin, Set.findMax]

susedi :: [V2 Int] -> Maybe (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
susedi dirs limits s = filter f $ map (+ s) dirs
  where
    f x = case limits of
      Nothing -> True
      Just bnds -> inBounds bnds x

susedi4 :: Maybe (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
susedi4 = susedi [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]

susedi8 :: Maybe (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
susedi8 = susedi [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1), V2 1 1, V2 (-1) 1, V2 1 (-1), V2 (-1) (-1)]

inBounds :: (V2 Int, V2 Int) -> V2 Int -> Bool
inBounds (V2 i0 j0, V2 i1 j1) (V2 i j) = (i0, i1) `inRange` i && (j0, j1) `inRange` j

generateGraph :: Map (V2 Int) Int -> (Gr Int Int, Map (V2 Int) Int)
generateGraph mapa = (mkGraph nodes edges, indexMap)
  where
    coords = Set.toList $ Map.keysSet mapa
    indexMap = Map.fromList $ zip coords [1 ..]
    nodes = zipWith (\coord index -> (index, mapa Map.! coord)) coords [1 ..]
    edges = concatMap f coords
    f coord = mapMaybe g (susedi4 Nothing coord)
      where
        g sused = do
          tip <- Map.lookup sused mapa
          guard (tip > 0)
          return (indexMap Map.! coord, indexMap Map.! sused, 1)

emptyLine :: String -> Bool
emptyLine = all isSpace

listToArray :: [e] -> Array Int e
listToArray lst = listArray (0, length lst - 1) lst

listCount :: (a -> Bool) -> [a] -> Int
listCount f = length . filter f

scale :: (Integral a, Integral b) => a -> V2 b -> V2 b
scale s (V2 x y) = V2 (fromIntegral s * x) (fromIntegral s * y)

argmin :: Ord a => [a] -> Int
argmin xs = fromJust $ elemIndex (minimum xs) xs

argmax :: Ord a => [a] -> Int
argmax xs = fromJust $ elemIndex (maximum xs) xs

freqIntMap :: [Int] -> IntMap Int
freqIntMap = IntMap.fromListWith (+) . flip zip (repeat 1)

-- | Returns a map of element frequencies from input list
freqMap :: Ord a => [a] -> Map a Int
freqMap = Map.fromListWith (+) . flip zip (repeat 1)

-- | stopCondition takes two consecutive values and returns whether to
-- stop the iteration or not
iterateUntil :: ((a, a) -> Bool) -> (a -> a) -> a -> [a]
iterateUntil stopCondition f start = let next = f start
                                      in if stopCondition (start, next)
                                            then [start,next]
                                            else start : iterateUntil stopCondition f next

traceVar :: Show a => a -> a
traceVar x = trace (show x) x

arrLookupWithDefault :: Arr.Ix i => e -> Arr.Array i e -> i -> e
arrLookupWithDefault d arr p = if inRange (Arr.bounds arr) p
                     then arr Arr.! p
                     else d

mkUniq :: (Ord a) => [a] -> [a]
mkUniq = map head . group . sort

showAnimation :: [String] -> IO ()
showAnimation strs = forM_ strs $ \str -> do
  clearScreen
  putStr str
  threadDelay 1000000

swap (a, b) = (b, a)

groupByn :: Int -> [a] -> [[a]]
groupByn _ [] = []
groupByn n l = take n l : groupByn n (drop n l)

whenM :: Bool -> b -> Maybe b
whenM True x = Just x
whenM False _ = Nothing

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x : xs) = makePairs xs ++ map (x, ) xs

iterateWithCycle :: Ord a => Int -> (a -> a) -> a -> a
iterateWithCycle num fun x0 = xf
  where
    xf = if ind1 == num then xs !! num else
      let cyc_len = ind1 - ind0
       in xs !! (ind0 + (num-ind0) `mod` cyc_len)
    (ind0, ind1) = go Map.empty $ zip xs [0..]
    go last_seen ((xi, ind) : rest) = if num == ind
                                         then (0, ind)
                                         else case Map.lookup xi last_seen of
                                                Just last_ind -> (last_ind, ind)
                                                Nothing -> go (Map.insert xi ind last_seen) rest
    xs = iterate fun x0
