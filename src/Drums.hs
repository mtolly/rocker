module Drums where

import System.MIDI

data Command
  = Kick
  | Snare
  | TomY
  | TomB
  | TomG
  | CymY
  | CymB
  | CymG
  | HihatFoot
  | HihatClose
  | HihatOpen
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

pitches :: Command -> [Int]
pitches c = case c of
  Kick -> [33, 35, 36]
  Snare -> [38, 31, 34, 37, 39, 40]
  TomY -> [48, 50]
  TomB -> [45, 47]
  TomG -> [41, 43]
  CymY -> [22, 26, 42, 46, 54]
  CymB -> [51, 53, 56, 59]
  CymG -> [49, 52, 55, 57]
  HihatFoot -> [44]
  HihatClose -> []
  HihatOpen -> []

sendCommand :: Command -> MidiMessage'
sendCommand d = case d of
  HihatClose -> CC 4 127
  HihatOpen -> CC 4 0
  _ -> NoteOn (head $ pitches d) 96

receiveCommand :: MidiMessage' -> Maybe Command
receiveCommand mm = case mm of
  NoteOn k v | v /= 0 -> lookup k $ do
    c <- [minBound .. maxBound]
    p <- pitches c
    return (p, c)
  CC 4 127 -> Just HihatClose
  CC 4 _   -> Just HihatOpen
  _ -> Nothing
