module Day09 (main09) where

import Util
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), ViewR(..), ViewL(..))
import Data.Char (ord)

solveA xs = rek (l, 0)
  where
    l = Seq.fromList $ concatMap f (zip [0..] xs)
    f (y, times) = let (id, kind) = y `divMod` 2
       in if kind == 0 then replicate times (Just id) else replicate times Nothing
    rek (l, ind) = case Seq.viewl l of
              Seq.EmptyL -> 0
              (Nothing :< l') -> case Seq.viewr (Seq.dropWhileR isNothing l') of
                                 Seq.EmptyR -> 0
                                 (l'' :> Just b) -> ind * b + rek (l'', ind+1)
              (Just a :< l') -> ind * a + rek (l', ind+1)

solveB xs = score finseq
  where
    score s = let 
                f (cum, ind) = \case
                  (y, Nothing) -> (cum, ind + y)
                  (y, Just a) -> (cum + (a * y * (ind + (ind+y-1)) `div` 2), ind + y)
               in foldl f (0, 0) s

    finseq = foldr apply (Seq.fromList l) l

    apply (_, Nothing) s = s
    apply el@(times, Just a) s =
      let good (y, Nothing) = y >= times
          good (_, Just a') = a == a'
          Just p = Seq.findIndexL good s
          Just (y, v) = Seq.lookup p s
       in if isJust v then s
                      else Seq.insertAt p el (
                              Seq.adjust (\(y, v) -> (y - times, v)) p (
                                Seq.adjust (\(y, _) -> (y, Nothing)) (fromJust $ Seq.elemIndexL el s) s))

    l = map f (zip [0..] xs)
    f (y, times) = let (id, kind) = y `divMod` 2
       in if kind == 0 then (times, Just id) else (times, Nothing)

main09 :: IO ()
main09 = do
    input <- map (subtract (ord '0') . ord) . head . lines <$> readFile "res/input09"
    print $ solveA input
    print $ solveB input
