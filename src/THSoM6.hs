module THSoM6 where

import Euterpea

import Data.List (sort)
import Data.Maybe (mapMaybe)

canonicalPitchClass :: PitchClass -> PitchClass
canonicalPitchClass Cff = As
canonicalPitchClass Cf = B
canonicalPitchClass Dff = C
canonicalPitchClass Df = Cs
canonicalPitchClass Css = D
canonicalPitchClass Eff = D
canonicalPitchClass Ef = Ds
canonicalPitchClass Fff = Ds
canonicalPitchClass Dss = E
canonicalPitchClass Ff = E
canonicalPitchClass Gff = E
canonicalPitchClass Ess = G
canonicalPitchClass Gf = Es
canonicalPitchClass Fss = G
canonicalPitchClass Aff = G
canonicalPitchClass Af = Gs
canonicalPitchClass Gss = A
canonicalPitchClass Bff = A
canonicalPitchClass Bf = As
canonicalPitchClass Ass = B
canonicalPitchClass Bss = Cs
canonicalPitchClass x = x

--properRow :: Music Pitch -> Bool
properRow music =
  let line = lineToList music
      toPitchMaybe (Prim (Note _ p)) = Just p
      toPitchMaybe _ = Nothing
      pitches = map (canonicalPitchClass . fst) $  mapMaybe toPitchMaybe line
  in sort pitches == [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
