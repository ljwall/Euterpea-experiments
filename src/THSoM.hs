module THSoM where

import Euterpea

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (isNothing)
import Euterpea.IO.MIDI.MidiIO (getAllDevices)

-- | Bit of a Hack---not really using these instuments, this is just a
-- way to fix midi channels. Tried `CustomInstrument String`, but it seems
-- to cause problems with the Enum instance for InstrumentName
midi1 :: InstrumentName
midi1 = AcousticGrandPiano

midi2 :: InstrumentName
midi2 = TenorSax

midi3 :: InstrumentName
midi3 = Bagpipe

channelMapFun :: ChannelMapFun
channelMapFun = predefinedCP [(midi1, 0),
                              (midi2, 1),
                              (midi3, 2)]

-- | Attempt to get the output device ID of the Roldan MC-101
mc101Device :: IO (Maybe OutputDeviceID)
mc101Device = do
  (_, outputDevs) <- getAllDevices
  return . fmap fst . find (\(_, dev) -> name dev == "MC-101 MIDI 1") $ outputDevs

-- | Custom play, using own Midi channel map and MC-101 device
play' :: (ToMusic1 a, NFData a) => Music a -> IO ()
play' music = do
  dev <- mc101Device
  when (isNothing dev) $ putStrLn "MC-101 device not found"
  playC defParams{devID = dev, perfAlg=fixPerf, chanPolicy=channelMapFun} music
    where
      fixPerf = map (\e -> e{eDur = max 0 (eDur e - 0.000001)}) . perform

-- | Test things...
bar :: Music Pitch
bar =  instrument midi1 $ c 3 wn :=:  e 3 wn  :=: g 3 wn

foo :: Music Pitch
foo =  instrument midi2 $ c 4 qn :+:  e 4 qn  :+: g 4 qn :+: a 4 qn

baz :: Music Pitch
baz = foo :=: bar

t251 :: Music Pitch
t251 = let dmin = d 4 hn :=: f 4 hn :=: a 4 hn
           gmaj = g 4 hn :=: b 4 hn :=: d 5 hn
           cmaj = c 4 wn :=: e 4 wn :=: g 4 wn
       in dmin :+: gmaj :+: cmaj

-- | Construct a ii-V-I progression
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne sd1 d = two :+: five :+: one
  where
    sd2 = trans 2 sd1
    sd3 = trans 2 sd2
    sd4 = trans 1 sd3
    sd5 = trans 2 sd4
    sd6 = trans 2 sd5
    sd7 = trans 2 sd6
    sd8 = trans 1 sd7
    sd9 = trans 2 sd8
    two = note d sd2 :=: note d sd4 :=: note d sd6
    five = note d sd5 :=: note d sd7 :=: note d sd9
    one = note (d*2) sd1 :=: note (d*2) sd3 :=: note (d*2) sd5


-- Ex 2.2

data BluesPitchClass = Ro | MT | Fo | Fi | MS
type BluesPitch = (BluesPitchClass, Int)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms  o d = note d (MS, o)

fromBluesPitch :: BluesPitch -> Pitch
fromBluesPitch (c, d) = (f c, d)
  where
    f Ro = C
    f MT = Ef
    f Fo = F
    f Fi = G
    f MS = Bf


fromBlues :: Music BluesPitch -> Music Pitch
fromBlues = fmap fromBluesPitch

bluesScale :: Music BluesPitch
bluesScale =  foldr1 (:+:) [ro 4 qn, mt 4 qn, fo 4 qn, fi 4 qn, ms 4 qn, ro 5 qn, ms 4 qn, fi 4 qn, fo 4 qn, mt 4 qn, ro 4 hn]


-- Ex 3.12
--
mkScale :: Pitch -> [Int] -> [Music Pitch]
mkScale p ints = map (note qn) pitches
  where prefix (mp:mps) i = pitch (absPitch mp + i):mp:mps
        pitches = reverse $ foldl prefix [p] ints

-- Ex 3.13
genScale :: Pitch -> Mode -> [Music Pitch]
genScale p mode = mkScale p intervals
  where intervals = rotate [2, 2, 1, 2, 2, 2, 1] (i mode)
        rotate xs n = take (length xs) . drop n $ cycle xs
        i Major = 0
        i Ionian = 0
        i Dorian = 1
        i Phrygian = 2
        i Lydian = 3
        i Mixolydian = 4
        i Aeolian = 5
        i Minor = 5
        i Locrian = 6
        i (CustomMode _)  = error "CutomMode not implmentd"


prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x]:map (x:) (prefixes xs)


prefix :: [Music a] -> Music a
prefix mel =
  let line1 = instrument midi1 . line . concat . prefixes $ mel
      line2 = instrument midi2 . transpose 12 . line . concat . prefixes . reverse $ mel
      m = line1 :=: line2
  in m :+: transpose 5 m :+: m

mel1 =[c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 =[c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]
