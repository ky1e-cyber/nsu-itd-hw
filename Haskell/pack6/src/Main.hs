{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Exception
import Data.Function
import Data.List
import Data.Map

instance {-# OVERLAPPING #-} Show String where
  show x = ['"'] ++ x ++ ['"']

data Gender = F | M
  deriving (Show, Eq)

listName =
  ["Оля", "Женя", "Катя", "Женя", "Паша", "Саша", "Саша", "Маша", "Надя", "Юля"] :: [String]

listID =
  [131, 132, 134, 135, 136, 137, 138, 139, 140, 141] :: [Int]

listGender =
  [F, F, F, M, M, M, F, F, F, F] :: [Gender]

listAge =
  [17, 18, 17, 19, 18, 19, 21, 19, 18, 20] :: [Int]

listHasFriends =
  [True, False, True, False, False, True, False, True, True, False] :: [Bool]

type PersonRecord =
  (String, Gender, Int, Bool)

mapAll :: Map Int PersonRecord
mapAll =
  fromList
    ( zip
        listID
        (zip4 listName listGender listAge listHasFriends)
    )

mapMale :: Map Int PersonRecord
mapMale = Data.Map.filter (\(_, gend, _, _) -> gend == M) mapAll

listIdMales :: [(Int, String)]
listIdMales =
  foldrWithKey (\id (name, _, _, _) acc -> (id, name) : acc) [] mapMale

countItems :: (Ord a) => [a] -> [(a, Int)]
countItems [] = []
countItems xs =
  let helper acc xs' =
        case xs' of
          [] -> acc
          xsHead : xsTail ->
            helper (insertWith (+) xsHead 1 acc) xsTail
   in foldrWithKey
        (\elem count acc -> (elem, count) : acc)
        []
        (helper Data.Map.empty xs)

main :: IO ()
main = do
  print mapAll
  putStrLn ""
  print listIdMales

  let !_ = assert (countItems ([] :: [Int]) == [])
  let !_ = assert (countItems ([1]) == [(1, 1)])
  let !_ =
        assert
          ( countItems
              ((repeat [1, 0] & Data.List.take 1000) >>= id)
              == ([(0, 1000), (1, 1000)])
          )

  return ()
