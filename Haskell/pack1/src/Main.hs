module Main where

import Data.Function
import Numeric.Natural

assertClose :: (Num a, Ord a, Show a) => a -> a -> a -> Natural -> IO ()
assertClose got expected eps testNum =
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

f151 :: Double -> Double
f151 x = (x ** 2) / 2

f152 :: Double -> Double
f152 x = sqrt (3 * x - (x ** 3))

f154a :: Double -> Double
f154a x = logBase 10 (x ** 2 - 4)

f165'3 :: Double -> Double
f165'3 x
  | (x >= 0 && x <= 2 * pi) =
      (sin 2 * x & sqrt) - (sin 3 * x & sqrt)
  | otherwise = error "Wrong argument to f165'3"

f167 :: Double -> Double
f167 x = logBase 10 (1 - 2 * (cos x))

f411c :: Natural -> Double
f411c n =
  fromIntegral (n ^ 2 - 1) / fromIntegral (2 * (n ^ 2) - n - 1)

f431 :: Natural -> Double
f431 n =
  fromIntegral (sum [(2 * i - 1) ^ 2 | i <- [1 .. n]])
    / fromIntegral (sum [(2 * i) ^ 2 | i <- [1 .. n]])

f433 :: Natural -> Double
f433 n =
  fromIntegral (sum [(3 * i - 2) ^ 3 | i <- [1 .. n]])
    / (fromIntegral (sum [3 * i - 2 | i <- [1 .. n]]) ^ 2)

f411a :: Double -> Double
f411a x =
  (x ** 2 - 1) / (2 * (x ** 2) - x - 1)

f411b :: Double -> Double
f411b x =
  (x ** 2 - 1) / (2 * (x ** 2) - x - 1)

limSeqKnown :: (Natural -> Double) -> Double -> Double -> Double
limSeqKnown f limActual eps =
  let tail i acc =
        if abs (acc - limActual) <= eps
          then acc
          else tail (i + 1) (f (i + 1))
   in tail 0 0

limFunPositiveArgsKnown :: (Double -> Double) -> Double -> Double -> Double -> Double
limFunPositiveArgsKnown f aproch limActual eps =
  let arg i = (aproch * i) / (i + 1)
      tail i acc =
        if abs (acc - limActual) <= eps
          then acc
          else tail (i + 1) (arg (i + 1) & f)
   in tail 0 0

limSeqDecUnknown :: (Natural -> Double) -> Double -> Double
limSeqDecUnknown f eps =
  let tail i acc =
        let nxt = f (i + 1)
         in if abs (nxt / acc) >= (1.0 - eps)
              then nxt
              else tail (i + 1) nxt
   in tail 0 0

lim411c :: Double -> Double
lim411c = limSeqKnown f411c 0.5

lim411b :: Double -> Double
lim411b = limFunPositiveArgsKnown f411b 1 (2 / 3)

lim411cUnkown :: Double -> Double
lim411cUnkown = limSeqDecUnknown f411c

main :: IO ()
main = do
  let eps = 0.0001
  assertClose (lim411b eps) (2 / 3) eps 1
  assertClose (lim411c eps) 0.5 eps 2
  assertClose (lim411cUnkown eps) 0.5 0.01 3 -- somewhat close idk
