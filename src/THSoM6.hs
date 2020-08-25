module THSoM6 where

import Euterpea

import Data.List (sort)
import Data.Maybe (mapMaybe)

canonicalPitchClass :: PitchClass -> PitchClass
canonicalPitchClass p = fst . pitch . absPitch $ (p, 4)


properRow :: Music Pitch -> Bool
properRow music =
  let line = lineToList music
      toPitchMaybe (Prim (Note _ p)) = Just p
      toPitchMaybe _ = Nothing
      pitches = map (canonicalPitchClass . fst) $  mapMaybe toPitchMaybe line
  in sort pitches == [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
