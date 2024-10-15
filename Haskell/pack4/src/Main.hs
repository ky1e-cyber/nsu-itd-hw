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

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs ys =
  let fill acc rest =
        case rest of
          [] -> acc
          hd : tl -> fill (hd : acc) tl

      helper :: (Ord a) => [a] -> [a] -> [a] -> [a]
      helper acc xs' ys' =
        case (xs', ys') of
          ([], []) -> acc
          (xs', []) -> fill acc xs'
          ([], ys') -> fill acc ys'
          (xHd : xTl, yHd : yTl) -> if xHd < yHd then helper (xHd : acc) xTl ys' else helper (yHd : acc) xs' yTl
   in reverse (helper [] xs ys)

count :: (Eq a) => [a] -> a -> Natural
count xs x =
  foldl
    (\cnt nxt -> if nxt == x then cnt + 1 else cnt)
    0
    xs

majElement :: (Eq a) => [a] -> Maybe a
majElement xs =
  let helper :: (Eq a) => [a] -> a -> Natural -> Maybe a
      helper xs cand cnt =
        case xs of
          [] ->
            if cnt == 0 then Nothing else Just cand
          hd : tl ->
            if cnt == 0
              then helper tl hd (cnt + 1)
              else helper tl cand (if hd == cand then cnt + 1 else cnt - 1)
   in case xs of
        [] -> Nothing
        hd : tl ->
          case helper tl hd 1 of
            Nothing -> Nothing
            Just cand ->
              if fromIntegral (count xs cand) > ((length xs) `div` 2)
                then Just cand
                else Nothing

main :: IO ()
main = do
  putStrLn "merge tests:"
  let (e1, e2) :: ([Int], [Int]) = ([], []) 
  testEq (merge e1 e2) [] 1
  let sortedLst :: [Int] = take 200 [1..]
  let sortedLstEven :: [Int] = filter (\x -> x `mod` 2 == 0) (take 200 [1..])
  let sortedLstOdd :: [Int] = filter (\x -> x `mod` 2 /= 0) (take 200 [1..])
  testEq (merge e1 sortedLst) sortedLst 2
  testEq (merge sortedLstEven e1) sortedLstEven 3
  testEq (merge sortedLstEven sortedLstOdd) sortedLst 4
  testEq (merge sortedLstOdd sortedLstEven) sortedLst 5
   

  putStrLn "count test:"
  testEq (count [1, 2, 3, 4] 1) 1 1
  testEq (count (take 10 (repeat 10)) 10) 10 2

  putStrLn "majElement tests:"
  testEq (majElement ([] :: [Int])) Nothing 1
  testEq (majElement [1]) (Just 1) 2
  testEq (majElement ['a', 'a']) (Just 'a') 3
  testEq (majElement (take 200 (repeat 1))) (Just 1) 4
  testEq (majElement [3, 5, 3, 5, 5, 3]) Nothing 5
  testEq (majElement [3, 5, 3, 5, 5, 5]) (Just 5) 6
  testEq (majElement [5, 5, 3, 5, 5, 5, 3, 3, 5, 3, 3, 5]) (Just 5) 7
  return ()
