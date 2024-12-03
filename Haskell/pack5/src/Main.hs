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

-- № 0
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

-- № 1
sum1 :: Int
sum1 = foldl (+) 0 (elems arI)

sum2 :: Int
sum2 = foldl1 (+) arI

sum3 :: Int
sum3 = sum arI

-- № 2
sum4 :: Int
sum4 =
  foldl (\acc nxt -> acc + (arI ! nxt)) 0 (indices arI)

-- № 3
indEq :: [(Int, Int)]
indEq =
  filter (\(i1, i2) -> i1 + i2 == arI ! (i1, i2)) (indices arI)

vals :: [Int]
vals = map (\ind -> arI ! ind) indEq

-- № 4
replaced :: Array (Int, Int) Int
replaced = arI // [(i, 77) | i <- indEq]

-- № 5
normR2 :: (Double, Double) -> (Double, Double) -> Double
normR2 (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

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

-- № 6
meanNeighborsLst :: [Double] -> [Double]
meanNeighborsLst xs =
  let f prevElem acc xs' =
        case xs' of
          [] -> acc
          x1 : x2 : _ -> f x1 ((prevElem + x1 + x2) / 3.0 : acc) (drop 1 xs')
          x1 : [] -> f x1 ((prevElem + x1) / 2.0 : acc) (drop 1 xs')
   in case xs of
        [] -> []
        x : [] -> [x]
        x1 : x2 : tl -> reverse (f x1 [(x1 + x2) / 2.0] (drop 1 xs))

meanNeighbors :: Array Int Double -> Array Int Double
meanNeighbors arr =
  listArray (bounds arr) (meanNeighborsLst (elems arr))

-- № 8
newtype Vec3d
  = Vec3d (Int, Int, Int)
  deriving (Show, Eq)

newtype Matrix
  = Matrix (Array (Int, Int) Int)
  deriving (Show, Eq)

prod :: Matrix -> Vec3d -> Vec3d
prod (Matrix mat) (Vec3d (x1, x2, x3)) =
  let y1 = x1 * (mat ! (0, 0)) + x2 * (mat ! (0, 1)) + x3 * (mat ! (0, 2))
      y2 = x1 * (mat ! (1, 0)) + x2 * (mat ! (1, 1)) + x3 * (mat ! (1, 2))
      y3 = x1 * (mat ! (2, 0)) + x2 * (mat ! (2, 1)) + x3 * (mat ! (2, 2))
   in Vec3d (y1, y2, y3)

-- № 9
class Logic a where
  neg :: a -> a
  (&&&) :: a -> a -> a
  (|||) :: a -> a -> a

instance Logic String where
  neg "" = "42"
  neg _ = ""
  "" &&& _ = ""
  (_ : _) &&& s = s
  "" ||| s = s
  s ||| "" = s
  s ||| _ = s

main :: IO ()
main = do
  putStrLn "Tasks 1 - 4:"
  putStrLn ("arI = " ++ (show arI))
  putStrLn "Sums: "
  putStrLn ("using elems = " ++ (show sum1))
  putStrLn ("using foldl1 = " ++ (show sum2))
  putStrLn ("using sum = " ++ (show sum3))
  putStrLn ("using indicies with array access = " ++ (show sum4))
  putStrLn "arI ! (i, j) == i + j:"
  putStrLn ("filtered when cond is true = " ++ (show vals))
  putStrLn ("replaced with 77 = " ++ (show replaced))

  putStrLn "chainLenghtR2 tests:"

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

  testClose
    (chainLenghtR2 arr)
    22506.17872083843
    eps
    3

  putStrLn "meanNeighbors tests:"

  let emptyArr = listArray (0, -1) ([] :: [Double])
  let singleArr = listArray (0, 0) ([1.0] :: [Double])
  let twoArr = listArray (0, 1) ([0.42, 1.0] :: [Double])
  let twoRes = listArray (0, 1) ([0.71, 0.71] :: [Double])
  let indexedArr = listArray (5, 8) ([1.0, 1.69, 42.1, 1.21] :: [Double])
  let indexedRes = listArray (5, 8) ([1.345, 14.93, 15.0, 21.655] :: [Double])
  testEq (meanNeighbors emptyArr) emptyArr 1
  testEq (meanNeighbors singleArr) singleArr 2
  testEq (meanNeighbors twoArr) twoRes 3
  testEq (meanNeighbors indexedArr) indexedRes 4

  putStrLn "Matrix * Vec3d product tests:"

  let ident =
        Matrix
          ( array
              ((0, 0), (2, 2))
              [ ((0, 0), 1),
                ((0, 1), 0),
                ((0, 2), 0),
                ((1, 0), 0),
                ((1, 1), 1),
                ((1, 2), 0),
                ((2, 0), 0),
                ((2, 1), 0),
                ((2, 2), 1)
              ]
          )

  let zrot90 =
        Matrix
          ( array
              ((0, 0), (2, 2))
              [ ((0, 0), 0),
                ((0, 1), -1),
                ((0, 2), 0),
                ((1, 0), 1),
                ((1, 1), 0),
                ((1, 2), 0),
                ((2, 0), 0),
                ((2, 1), 0),
                ((2, 2), 1)
              ]
          )

  let mat =
        Matrix
          ( array
              ((0, 0), (2, 2))
              [ ((0, 0), 2),
                ((0, 1), 4),
                ((0, 2), 6),
                ((1, 0), 4),
                ((1, 1), 5),
                ((1, 2), 6),
                ((2, 0), 3),
                ((2, 1), 1),
                ((2, 2), -2)
              ]
          )

  let zeroMat =
        Matrix
          ( array
              ((0, 0), (2, 2))
              [ ((0, 0), 0),
                ((0, 1), 0),
                ((0, 2), 0),
                ((1, 0), 0),
                ((1, 1), 0),
                ((1, 2), 0),
                ((2, 0), 0),
                ((2, 1), 0),
                ((2, 2), 0)
              ]
          )

  let b1 = Vec3d (1, 0, 0)
  let b2 = Vec3d (0, 1, 0)
  let b3 = Vec3d (0, 0, 1)
  let vec = Vec3d (4, -2, 3)
  let zeroVec = Vec3d (0, 0, 0)

  testEq (prod ident b1) b1 1
  testEq (prod ident b2) b2 2
  testEq (prod ident b3) b3 3
  testEq (prod ident vec) vec 4
  testEq (prod zrot90 b1) b2 5
  testEq (prod mat vec) (Vec3d (18, 24, 4)) 6
  testEq (prod zeroMat b1) zeroVec 7
  testEq (prod zeroMat vec) zeroVec 8

  putStrLn "Logic String tests:"
  let false = ""
  testEq (neg "non empty") false 1
  testEq (neg false /= false) True 2
  testEq (false &&& "non-empty") false 3
  testEq ("69" &&& false) false 4
  testEq ("1111" &&& "111" /= false) True 5
  testEq (false &&& false) false 6
  testEq (false ||| "non-empty" /= false) True 7
  testEq ("69" ||| false /= false) True 8
  testEq ("1111" ||| "111" /= false) True 9
  testEq (false ||| false) false 10

  return ()
