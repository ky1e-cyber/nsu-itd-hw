{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception
import Data.Function
import Data.Map qualified as DMap

diffInitLargestSeq :: (Eq a) => [a] -> [a] -> ([a], [a])
diffInitLargestSeq xs ys =
  let helper xs' ys' =
        case (xs', ys') of
          ([], _) -> (xs', ys')
          (_, []) -> (xs', ys')
          (y : tys, x : txs) ->
            if (y == x)
              then helper tys txs
              else (xs', ys')
   in helper xs ys

splitPath :: String -> [String]
splitPath pth =
  let helper acc accStr rest =
        case rest of
          [] -> reverse accStr : acc
          hd : [] | hd == '/' -> reverse accStr : acc -- little hack to drop last '/'
          hd : tl ->
            if hd == '/'
              then helper (reverse accStr : acc) "" tl
              else helper acc (hd : accStr) tl
   in reverse (helper [] "" pth) & drop 1

makeRelPath :: String -> String -> String
makeRelPath abspath from =
  let (cutAbs, cutFrom) =
        diffInitLargestSeq
          (splitPath abspath)
          (splitPath from)
      backTr =
        ["../" | _ <- cutFrom]
          >>= id
          & reverse
          & drop 1
          & reverse
   in backTr ++ (cutAbs >>= (\x -> "/" ++ x))

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a)
  deriving (Show, Read, Eq)

data Tr = Nd Int Int | Lf
  deriving (Show, Read, Eq)

t :: DMap.Map Int Tr
t =
  DMap.fromList
    [ (21, Nd 12 34),
      (12, Nd 51 26),
      (51, Lf),
      (26, Lf),
      (34, Nd 17 30),
      (17, Lf),
      (30, Nd 73 27),
      (73, Lf),
      (27, Lf)
    ]

fndRoot :: DMap.Map Int Tr -> Int
fndRoot tMap =
  let helper candKey tMap' =
        if DMap.null tMap'
          then candKey
          else
            let nxtKey = (head (DMap.keys tMap'))
             in case (tMap' DMap.! nxtKey) of
                  Nd left right ->
                    let tMap'' = (DMap.delete left . DMap.delete right) tMap'
                     in if (left == candKey || right == candKey)
                          then helper nxtKey (DMap.delete nxtKey tMap'')
                          else helper candKey (DMap.delete nxtKey tMap'')
                  Lf -> helper candKey (DMap.delete nxtKey tMap')
   in case (DMap.keys tMap) of
        [] -> undefined
        nxtInd : _ -> helper nxtInd (DMap.delete nxtInd tMap)

main :: IO ()
main = do
  let !_ = assert ((diffInitLargestSeq [1 .. 5] [9 .. 11]) == ([1, 2, 3, 4, 5], [9, 10, 11]))
  let !_ = assert ((diffInitLargestSeq [1 .. 5] [1 .. 5]) == ([], []))
  let !_ = assert ((diffInitLargestSeq ([] :: [Int]) ([] :: [Int])) == (([] :: [Int]), ([] :: [Int])))
  let !_ = assert (makeRelPath "/dir1/dir2/dir5/file" "/dir1/dir2/dir3/dir4/" == "../../dir5/file")
  let !_ = assert (fndRoot t == 21)
  return ()
