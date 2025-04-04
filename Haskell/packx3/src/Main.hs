{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a)
  deriving (Show, Read)

instance Functor ATree where
  fmap f (ALeaf e) = ALeaf (f e)
  fmap f (ABranch left e right) =
    ABranch (fmap f left) (f e) (fmap f right)

dTree0 =
  ABranch
    (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 0 (ALeaf 66)) 22 (ALeaf 46)))
    1.1
    (ABranch (ALeaf 34) 3.3 (ALeaf 35))

dTree =
  ABranch
    (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46)))
    1.1
    (ABranch (ALeaf 34) 3.3 (ALeaf 35))

idTree :: ATree (Int, Double)
idTree =
  ABranch
    ( ABranch
        (ALeaf (4, 21))
        (2, 2.2)
        ( ABranch
            ( ABranch
                (ALeaf (100, 62))
                (7, 0)
                (ALeaf (200, 66))
            )
            (5, 22)
            (ALeaf (203, 46))
        )
    )
    (1, 1.1)
    ( ABranch
        (ALeaf (301, 34))
        (3, 3.3)
        (ALeaf (307, 0))
    )

dE :: ATree (Int, Double) -> Either String Double
dE (ALeaf (i, x)) =
  if x == 0
    then Left ("Operand of ID " ++ show i ++ " is 0")
    else Right x
dE (ABranch left (i, x) right) = do
  scur <- dE $ ALeaf (i, x)
  sleft <- dE left
  sright <- dE right
  return (scur * sleft * sright)

divWithTreeE :: Double -> ATree (Int, Double) -> Either String Double
divWithTreeE x t = do
  y <- dE t
  return (x / y)

-- might be better to use morphisms tho
divWithTreeM :: Double -> ATree Double -> Maybe Double
divWithTreeM x t =
  case divWithTreeE x ((0,) <$> t) of
    Left _ -> Nothing
    Right z -> Just z

treeId :: ATree (Int, a) -> Int
treeId t = case t of
  ALeaf (i, _) -> i
  ABranch _ (i, _) _ -> i

sumWithTreeE :: ATree (Int, Double) -> Writer String Double
sumWithTreeE (ALeaf (i, x)) =
  writer (x, "Got value " ++ show x ++ " from ID " ++ show i ++ ";\n")
sumWithTreeE (ABranch left (i, x) right) = do
  lleft <- sumWithTreeE left
  lright <- sumWithTreeE right
  let s = lleft + lright + x
  tell
    ( "Added sums from subtree ID "
        ++ show (treeId left)
        ++ " and subtree ID "
        ++ show (treeId right)
        ++ " with value from ID "
        ++ show i
        ++ " = "
        ++ show x
        ++ ", sum = "
        ++ show s
        ++ ";\n"
    )
  return s

numTreeS :: ATree Double -> State Int (ATree (Int, Double))
numTreeS t = do
  cnt <- get
  put (cnt + 1)
  case t of
    ALeaf x -> return $ ALeaf (cnt, x)
    ABranch left x right -> do
      nleft <- numTreeS left
      nright <- numTreeS right
      return $ ABranch nleft (cnt, x) nright

numTree :: ATree Double -> ATree (Int, Double)
numTree t = flip evalState 0 $ numTreeS t

main :: IO ()
main = putStrLn "Hello, Haskell!"
