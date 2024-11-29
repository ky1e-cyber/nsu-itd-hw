module Main where

import Data.Array
import Numeric.Natural

testEq :: (Eq a, Show a) => a -> a -> Natural -> IO ()
testEq got expected testNum =
  if got == expected
    then putStrLn ("[SUCCESS] Test number: " ++ (show testNum))
    else
      putStrLn
        ( "[FAILED] Test number: "
            ++ (show testNum)
            ++ " Expected: "
            ++ (show expected)
            ++ " Got: "
            ++ (show got)
        )

testClose :: (Num a, Ord a, Show a) => a -> a -> a -> Natural -> IO ()
testClose got expected eps testNum =
  if ((abs (got - expected)) <= eps)
    then putStrLn ("[SUCCESS] Test number: " ++ (show testNum))
    else
      putStrLn
        ( "[FAILED] Test number: "
            ++ (show testNum)
            ++ " Expected close to: "
            ++ (show expected)
            ++ " Got: "
            ++ (show got)
        )

arI :: Array (Int, Int) Int
arI =
  array
    ((1, 1), (2, 3))
    [ ((1, 1), 8),
      ((1, 2), 11),
      ((1, 3), 4),
      ((2, 1), 3),
      ((2, 2), 2),
      ((2, 3), 4)
    ]

normR2 :: (Double, Double) -> (Double, Double) -> Double
normR2 (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

window :: [[a]] -> Int -> [a] -> [[a]]
window acc windowSize xs =
  case xs of
    [] -> reverse acc
    _ ->
      window
        (take (windowSize) xs : acc)
        windowSize
        (drop windowSize xs)

chainLenghtR2 :: (Ix i) => Array i (Double, Double) -> Double
chainLenghtR2 xs =
  case elems xs of
    [] -> 0.0
    hd : tl ->
      let f :: (Double, (Double, Double)) -> (Double, Double) -> (Double, (Double, Double))
          f acc nxt =
            let dist = normR2 (snd acc) nxt
             in (fst acc + dist, nxt)
       in fst (foldl f (0.0, hd) tl)

class Logic a where
  neg :: a -> a
  (&&&) :: a -> a -> a
  (|||) :: a -> a -> a

main :: IO ()
main = do
  -- № 1
  -- № 5
  let eps = 0.0000000001
  testClose
    (chainLenghtR2 (listArray (0, 0) ([] :: [(Double, Double)])))
    0.0
    eps
    0

  testClose
    (chainLenghtR2 (listArray (0, 0) [(0.42, 69.042)]))
    0.0
    eps
    1

  testClose
    (chainLenghtR2 (listArray (0, 2) [(0.0, 0.0), (1.42, 0.0), (1.42, 5.0)]))
    (1.42 + 5.0)
    eps
    2

  let arr =
        listArray
          (1, 5)
          ( [ ((-5.23), 4.2),
              (2.456, 13.234),
              (11241, 211.2),
              ((-1), 0),
              (8.3, 3.8)
            ] ::
              [(Double, Double)]
          )

  testClose
    (chainLenghtR2 arr)
    22506.17872083843
    eps
    3

  return ()
