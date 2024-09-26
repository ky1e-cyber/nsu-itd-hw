module Main where

import Data.Ratio
import Numeric.Natural

assert :: Bool -> ()
assert False = error "assertion failed!"
assert True = () 

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

sign :: Double -> Integer
sign x = if x < 0.0 then -1 else (if x > 0.0 then 1 else 0)

solver :: (Double -> Double) -> Segment -> Double -> Maybe Double
solver f seg eps
  | start seg > end seg = Nothing
  | sign (start seg) == sign (end seg) = Nothing
  | eps < 0.0 = Nothing
  | otherwise =
      let solverConfirmed f seg eps =
            let x = (start seg + end seg) / 2
                y = f x
             in if y <= eps
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
   in seriesTail x 1

arctanTaylorEps :: Double -> Double -> Double
arctanTaylorEps x eps =
  let seriesTail (acc :: Double) (i :: Natural) =
        let sign = (-1) ^^ (i + 1)
            denom = 2 * i - 1
            next = acc + sign * ((x ^^ denom) / fromIntegral denom)
         in if abs (next - acc) <= eps
              then next
              else seriesTail next (i + 1)
   in seriesTail x 1

main :: IO ()
main = do
  let f x = 2 ** x - x ^ 2
      eps = 0.001
      seg1 = Segment {start = -1, end = 1}
      seg2 = Segment {start = 1, end = 3}
      seg3 = Segment {start = 3, end = 5} 
      
    -- in assert ((solver f seg1 eps) - (2 * logBase 2 (-x) )
  

  return ()
