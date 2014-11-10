module Instruments where

import System.MIDI

data Drum
  = Kick
  | Snare
  | TomY
  | TomB
  | TomG
  | CymY
  | CymB
  | CymG
  | HihatFoot
  | HihatOpen Bool
  deriving (Eq, Ord, Show, Read)

drumMessage :: Drum -> MidiMessage
drumMessage d = case d of
  Kick        -> noteOn 33 -- 33, 35, 36
  Snare       -> noteOn 38 -- 38, 31, 34, 37, 39, 40
  TomY        -> noteOn 48 -- 48, 50
  TomB        -> noteOn 45 -- 45, 47
  TomG        -> noteOn 41 -- 41, 43
  CymY        -> noteOn 22 -- 22, 26, 42, 46, 54
  CymB        -> noteOn 51 -- 51, 53, 56, 59
  CymG        -> noteOn 49 -- 49, 52, 55, 57
  HihatFoot   -> noteOn 44 -- 44
  HihatOpen b -> MidiMessage 1 $ CC 4 $ if b then 0 else 127
  -- 127 is closed, any other value is open
  where noteOn k = MidiMessage 1 $ NoteOn k 96
