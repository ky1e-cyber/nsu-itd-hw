module Main where

-- sdvig :: [a] -> [[a]]
-- sdvig xs =
--   let pxs = take (length xs) (repeat xs)
--       shift xs ind =
--         let drop xs n i =
--               case xs of
--                 [] -> []
--                 x : txs -> if i == n then xs else drop txs n (i + 1)
--             tl = take ind xs
--             hd = drop xs ind 0
--         in hd ++ tl
--   in foldl (\acc nxt -> (shift (fst nxt) (snd nxt)) : acc) [] (zip pxs [(length xs) .. 0])

sdvig :: [a] -> [[a]]
sdvig xs =
  let drop lst n i =
        case lst of
          [] -> []
          hd : tl -> if i == n then lst else drop tl n (i + 1)
      shift lst n =
        let tl = take n lst
            hd = drop lst n 0
         in hd ++ tl
   in foldl
        (\acc (lst, n) -> (shift lst n) : acc)
        []
        (zip (take (length xs) (repeat xs)) (reverse [0 .. (length xs) - 1]))

farthestpt :: (Double, Double) -> [(Double, Double)] -> Double
farthestpt p0 pts =
  let dist (x1, y1) (x2, y2) =
        sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))
   in foldl
        ( \acc nxt ->
            let dst = dist p0 nxt
             in if (dst > acc) then dst else acc
        )
        0.0
        pts

rmRepeats :: (Eq a, Ord a) => [a] -> [a]
-- where xs is sorted
rmRepeats xs =
  case xs of
    [] -> []
    hd : tl ->
      reverse
        ( foldl
            ( \lst nxtElem ->
                if nxtElem == (head lst)
                  then lst
                  else (nxtElem : lst)
            )
            [hd]
            tl
        )

main :: IO ()
main = do
  print (sdvig [1, 2, 3])
  print (sdvig [5, 4, 10, 7, 7, 7])
  print (farthestpt (0.0, 0.0) [(-13, 0), (0, -13), (2, 2), (-20, -20)])
  print (rmRepeats [1, 1, 2, 3, 4, 4, 5, 5, 5])
  return ()
