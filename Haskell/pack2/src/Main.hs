module Main where

import Data.Ratio
import Numeric.Natural

data Segment = Segment
  { start :: Double,
    end :: Double
  }

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

solver :: (Double -> Double) -> Segment -> Double -> Double
solver f seg eps = undefined

arctanTaylorSteps :: Double -> Natural -> Double
arctanTaylorSteps x n = undefined

arctanTaylorEps :: Double -> Double -> Double
arctanTaylorEps x eps = undefined

main :: IO ()
main = print (fib 35)
