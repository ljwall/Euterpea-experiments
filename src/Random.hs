module Random where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import System.Random (getStdGen, randomRs)

import Euterpea

getItem :: Integral i
        => [(a, i)]
        -> i
        -> a
getItem xs i =
  let ys = foldr (\(x, j) ((x', j'):rest) ->  (x, j+j'):(x', j'):rest) [(undefined, 0)] $ filter (\(_, j) -> j>0) xs
      total = snd $ head ys
      i' = if (i `mod` total) >= 0 then i `mod` total else  (i `mod` total) + total
      index = fromJust $ findIndex (\(_, j) -> j<=i') ys
  in fst $  ys !! (index - 1)

randomWeighted :: [(a, Integer)]
               -> IO [a]
randomWeighted xs = do
  gen <- getStdGen
  let ints = (randomRs (0, sum $ map snd xs) gen)
  return . map (getItem xs) $ ints

noteWeights =
  [ (c 3, 4)
  , (d 3, 1)
  , (e 3, 4)
  , (f 3, 1)
  , (g 3, 4)
  , (a 3, 1)
  , (b 3, 4)
  , (c 4, 2)
  , (d 4, 2)
  , (e 4, 1)
  , (f 4, 0)
  , (g 4, 1)
  , (a 4, 0)
  , (b 4, 1)
  ]
