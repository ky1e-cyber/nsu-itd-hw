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

-- totally inefficent but i have no time and brainpower for
-- figuring out how to do it better
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
  let empt :: [Int] = []
  testEq (listEq empt empt) True 4
  testEq (listEq [1..] empt) False 5
  
  putStrLn "cantorPairs tests:"
  let cnt = 100
  testEq
    ( map
        (\(x, y) -> (x ^ 2 + x + 2 * x * y + 3 * y + y ^ 2) `div` 2)
        (take cnt cantorPairs)
    )
    [0 .. ((fromIntegral cnt) - 1)]
    1

  return ()
