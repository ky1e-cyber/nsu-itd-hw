module Main where

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

listEq :: (Eq a) => [a] -> [a] -> Bool
listEq lst1 lst2 =
  let eqZip :: (Eq a) => [a] -> [a] -> [Bool] -> [Bool]
      eqZip [] [] acc = acc
      eqZip [] (_ : _) acc = (False) : acc
      eqZip (_ : _) [] acc = (False) : acc
      eqZip (x1 : xs1) (x2 : xs2) acc =
        if (x1 == x2)
          then eqZip xs1 xs2 (True : acc)
          else False : acc
   in foldr
        (&&)
        True
        (eqZip lst1 lst2 [])

pythagoreanTriples :: [(Natural, Natural, Natural)]
pythagoreanTriples =
  [ (x, y, c)
  | c <- [1 ..],
    x <- [1 .. c],
    y <- [1 .. c],
    x ^ 2 + y ^ 2 == c ^ 2
  ]

-- primitivePythagoreanTriples :: [(Natural, Natural, Natural)]
-- primitivePythagoreanTriples =
-- let skip_coprimes coprime end = [x | x <- [1 .. end], x `gcd` coprime /= 1]
-- skip_coprimes2 coprime1 coprime2 end = [x | x <- [1 .. end], x `gcd` coprime1 /= 1, x `gcd` coprime2 /= 1]
-- in [(x, y, c) | c <- [1 ..], x <- skip_coprimes c c, y <- skip_coprimes2 x c c, x ^ 2 + y ^ 2 == c ^ 2]

perfectNumbers :: [Natural]
perfectNumbers =
  let divisors x = [i | i <- [x - 1, x - 2 .. 1], x `mod` i == 0]
   in [x | x <- [1 ..], sum (divisors x) == x]

cantorPairs :: [(Natural, Natural)]
cantorPairs =
  let reverse_pi :: Natural -> (Natural, Natural)
      reverse_pi n =
        let y = n - t
            x = w - y
         in (x, y)
        where
          w = floor ((sqrt (8 * (fromIntegral n) + 1) - 1) / 2.0)
          t = (((w ^ 2) + w) `div` 2)
   in [reverse_pi i | i <- [0, 1 ..]]

minNorm :: [(Double, Double)] -> Double
minNorm vs =
  let norms :: [Double]
      norms = map (\v -> sqrt ((fst v) ** 2 + (snd v) ** 2)) vs
   in foldl
        (\cmin nxt -> if nxt < cmin then nxt else cmin)
        (1.0 / 0.0)
        norms

main :: IO ()
main = do
  putStrLn "listEq tests:"
  testEq (listEq [1, 2, 3, 4] [1, 2, 3, 4]) True 1
  testEq (listEq [1, 2, 3] [1, 2, 3, 4]) False 2
  testEq (listEq (repeat 1) ([1, 1, 1, 1] ++ [1 ..])) False 3
  putStrLn "pythagoreanTriples tests:"
  let isPythTripple (x, y, c) = x ^ 2 + y ^ 2 == c ^ 2
  testEq (foldr (&&) True (map isPythTripple (take 20 pythagoreanTriples))) True 1

  let isPerfect x = x == sum [d | d <- [1 .. x - 1], x `mod` d == 0]
  putStrLn "perfectNumbers tests:"
  testEq (foldr (&&) True (map isPerfect (take 20 perfectNumbers))) True 1

  putStrLn "cantorPairs tests:"
  testEq (take 6 cantorPairs) [(0, 0), (0, 1), (1, 0), (0, 2), (1, 1), (2, 0)] 1

  putStrLn "minNorm tests"

  testEq (minNorm [(0.0, 0.001), (pi, 42.0), (-1.0, -1.0)]) 0.0 1
  testEq (minNorm [(0.0, 0.0), (0.0, 0.0), (-1.0, -1.0)]) 0.0 2

  return ()
