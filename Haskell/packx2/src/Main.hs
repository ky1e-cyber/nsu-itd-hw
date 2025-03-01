module Main
  ( main,
    findMothernalGrandFather,
    findMothernalGrandGrandFather,
    findAllParents,
    findAllGrandParents,
    isOrphan,
    findSelectedFather,
  )
where

import qualified Barans
import Control.Monad
import Data.Either
import Data.Maybe

findMothernalGrandFather :: Barans.Sheep -> Maybe Barans.Sheep
findMothernalGrandFather s =
  Barans.mother s >>= Barans.father

findMothernalGrandGrandFather :: Barans.Sheep -> Maybe Barans.Sheep
findMothernalGrandGrandFather s =
  findMothernalGrandGrandFather s >>= Barans.father

findAllParents :: Barans.Sheep -> [Barans.Sheep]
findAllParents s =
  catMaybes [Barans.mother s, Barans.father s]

findAllGrandParents :: Barans.Sheep -> [Barans.Sheep]
findAllGrandParents s =
  join $ map findAllParents (findAllParents s)

isOrphan :: Barans.Sheep -> Bool
isOrphan s = null $ findAllParents s

selectedBarans :: [Barans.Sheep]
selectedBarans = ["i3", "i5", "i6", "i9", "i12"]

findSelectedFather :: Barans.Sheep -> Maybe Barans.Sheep
findSelectedFather s =
  Barans.father s >>= (\it -> lookup it $ map (\x -> (x, x)) selectedBarans)

findNearestSelectedFather' ::
  Barans.Sheep -> Either (Maybe Barans.Sheep) Barans.Sheep
findNearestSelectedFather' s =
  maybe
    (maybe (Left Nothing) Right $ Barans.father s)
    (Left . Just)
    (findSelectedFather s)

findNearestSelectedFather'' ::
  Barans.Sheep -> Either (Maybe Barans.Sheep) Barans.Sheep
findNearestSelectedFather'' s =
  findNearestSelectedFather' s >>= findNearestSelectedFather''

findNearestSelectedFather :: Barans.Sheep -> Maybe Barans.Sheep
findNearestSelectedFather s =
  fromLeft undefined $ findNearestSelectedFather'' s

main :: IO ()
main = do
  return ()
