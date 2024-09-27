module Main where

import Data.Function
import Data.Ratio
import Numeric.Natural

-- Utils
unwrap :: Maybe a -> a
unwrap Nothing = error "Unwrapped Nothing"
unwrap (Just x) = x

sign :: Double -> Integer
sign x = if x < 0.0 then -1 else (if x > 0.0 then 1 else 0)

assertEq :: (Eq a, Show a) => a -> a -> Natural -> IO ()
assertEq got expected testNum =
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

--

fibSimple :: Natural -> Natural
fibSimple 0 = 0
fibSimple 1 = 1
fibSimple n = (fibSimple (n - 2)) + (fibSimple (n - 1))

fib :: Natural -> Natural
fib 0 = 0
fib 1 = 1
fib n = _fib 1 0 1
  where
    _fib :: Natural -> Natural -> Natural -> Natural
    _fib i prev acc =
      if i == n
        then acc
        else _fib (i + 1) acc (prev + acc)

harmsum :: Natural -> Rational
harmsum n =
  sum [1 / toRational i | i <- [1 .. n]]

data Segment = Segment
  { start :: Double,
    end :: Double
  }

solver :: (Double -> Double) -> Segment -> Double -> Maybe Double
solver f seg eps
  | start seg > end seg = Nothing
  | sign (f (start seg)) == sign (f (end seg)) = Nothing
  | eps < 0.0 = Nothing
  | otherwise =
      let solverConfirmed f seg eps =
            let x = (start seg + end seg) / 2
                y = f x
             in if abs y <= eps
                  then x
                  else
                    if y > 0.0
                      then solverConfirmed f (Segment {start = (start seg), end = x}) eps
                      else solverConfirmed f (Segment {start = x, end = (end seg)}) eps
       in Just (solverConfirmed f seg eps)

arctanTaylorSteps :: Double -> Natural -> Double
arctanTaylorSteps x n =
  let seriesTail (acc :: Double) (i :: Natural) =
        if i == n
          then acc
          else
            let sign = (-1) ^^ (i + 1)
                denom = (2 * i - 1)
             in seriesTail (acc + sign * ((x ^^ denom) / fromIntegral denom)) (i + 1)
   in seriesTail 0 1

arctanTaylorEps :: Double -> Double -> Double
arctanTaylorEps x eps =
  let seriesTail (acc :: Double) (i :: Natural) =
        let sign = (-1) ^^ (i + 1)
            denom = 2 * i - 1
            next = acc + sign * ((x ^^ denom) / fromIntegral denom)
         in if abs (next - acc) <= eps
              then next
              else seriesTail next (i + 1)
   in seriesTail 0 1

main :: IO ()
main = do
  putStrLn "fibSimple tests:"
  assertEq (fibSimple 0) 0 0
  assertEq (fibSimple 1) 1 1
  assertEq (fibSimple 40) 102334155 2

  putStrLn "fib tests:"
  assertEq (fib 0) 0 1
  assertEq (fib 1) 1 2
  assertEq (fib 100) 354224848179261915075 3

  putStrLn "harmsum tests:"
  assertEq (harmsum 1) 1 1
  assertClose (harmsum 2) 1.5 0.001 2
  assertClose (harmsum 8) 2.718 0.001 3
  assertClose (harmsum (10 ^ 3)) 7.485 0.001 4

  putStrLn "solver tests:"
  let f x = 2 ** x - x ^ 2
      eps = 0.00001
      seg1 = Segment {start = -1, end = 1}
      seg2 = Segment {start = 1, end = 3}
      seg3 = Segment {start = 3, end = 5}
  assertClose (solver f seg1 eps & unwrap) (-0.766665) eps 1
  assertClose (solver f seg2 eps & unwrap) 2.0 eps 2
  assertClose (solver f seg3 eps & unwrap) 4.0 eps 3

  let eps = 0.000001

  putStrLn "arctanTaylorSteps tests:"
  assertClose (arctanTaylorSteps 0.4 25) 0.380506 eps 1

  putStrLn "arctanTaylorEps tests:"
  assertClose (arctanTaylorEps 0.4 eps) 0.380506 eps 1
