{-# LANGUAGE ScopedTypeVariables #-}

module Turing where

import System.Random
import Control.Monad


data Bit = High | Low
  deriving (Show)

toggle :: Bit -> Bit
toggle High = Low
toggle Low = High


data ShiftRegister8 =  ShiftRegister8 Bit Bit Bit Bit Bit Bit Bit Bit
  deriving (Show)

class ShiftRegister a where
  rotate :: Bit -> a -> a
  randomState :: IO a

instance ShiftRegister ShiftRegister8 where
  rotate High (ShiftRegister8 a b c d e f g h) = ShiftRegister8 (toggle h) a b c d e f g
  rotate Low (ShiftRegister8 a b c d e f g h) = ShiftRegister8 h a b c d e f g
  randomState = do
    [a, b, c, d, e, f, g, h] <- bits 0.5 8
    return $ ShiftRegister8 a b c d e f g h

bits :: Float -> Int ->  IO [Bit]
bits p n = sequence . take n . repeat $ do
  (val::Float) <- randomRIO (0, 1)
  return $ if val < p then Low else High

run :: (ShiftRegister s)
    => [Bit]
    -> s
    -> [s]
run bs init = foldr (\bit (s:rest) -> (rotate bit s):s:rest) [init] bs
