module Main where

import System.MIDI
import Control.Monad
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)

withSource :: Source -> Maybe ClientCallback -> (Connection -> IO a) -> IO a
withSource src cc = bracket (openSource src cc) close

withDestination :: Destination -> (Connection -> IO a) -> IO a
withDestination dst = bracket (openDestination dst) close

getSource :: IO Source
getSource = do
  let isQ49 = fmap (== "Q49") . getName
  srcs <- enumerateSources >>= filterM isQ49
  case srcs of
    [src] -> return src
    _ -> error $ "getSource: found " ++ show srcs

getDestination :: IO Destination
getDestination = do
  let isSport = fmap (== "Port 1") . getName
  dsts <- enumerateDestinations >>= filterM isSport
  case dsts of
    [dst] -> return dst
    _ -> error $ "getDestination: found " ++ show dsts

data Drum = Kick | Snare | TomY | TomB | TomG | CymY | CymB | CymG
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

playDrum :: Drum -> Connection -> IO ()
playDrum d con = let
  pitch = case d of
    Kick  -> 33
    Snare -> 38
    TomY  -> 48
    TomB  -> 45
    TomG  -> 41
    CymY  -> 22
    CymB  -> 51
    CymG  -> 49
  in send con $ MidiMessage 1 $ NoteOn pitch 96

q49ToDrum :: Int -> Maybe Drum
q49ToDrum p = let
  rightDrums = [Kick, CymY, Snare, TomY, CymB, TomB, CymG, TomG]
  rightHand = zip [45 ..] rightDrums
  leftHand = zip [43, 42 ..] rightDrums
  in lookup p $ rightHand ++ leftHand

main :: IO ()
main = do
  src <- getSource
  dst <- getDestination
  withSource src Nothing $ \scon ->
    withDestination dst $ \dcon -> do
      start scon
      fix $ \loop -> do
        evt <- getNextEvent scon
        case evt of
          Just (MidiEvent _ (MidiMessage _ (NoteOn k _))) -> case q49ToDrum k of
            Just d  -> do
              playDrum d dcon
              print d
            Nothing -> return ()
          Just (MidiEvent _ (MidiMessage _ (CC 64 127))) -> do
            playDrum Kick dcon
            print Kick
          _ -> return ()
        threadDelay 100 >> loop
