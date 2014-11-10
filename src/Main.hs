module Main where

import System.MIDI
import Control.Monad
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Text.Read (readMaybe)
import System.Exit (exitFailure)

withSource :: Source -> Maybe ClientCallback -> (Connection -> IO a) -> IO a
withSource src cc = bracket (openSource src cc) close

withDestination :: Destination -> (Connection -> IO a) -> IO a
withDestination dst = bracket (openDestination dst) close

promptInt :: String -> [(Int, a)] -> IO a
promptInt prompt choices = do
  hSetBuffering stdout NoBuffering
  fix $ \loop -> do
    putStr prompt
    putChar ' '
    ln <- getLine
    case readMaybe ln of
      Nothing -> putStrLn "Type a number." >> loop
      Just i -> case lookup i choices of
        Nothing -> putStrLn "Not an option." >> loop
        Just x -> return x

getSource :: IO Source
getSource = do
  srcs <- fmap (zip [1..]) enumerateSources
  when (null srcs) $ do
    putStrLn "Couldn't find any MIDI sources."
    exitFailure
  putStrLn "Sources:"
  forM_ srcs $ \(i, src) -> do
    name <- getName src
    putStrLn $ show i ++ ") " ++ name
  promptInt "Which source?" srcs

getDestination :: IO Destination
getDestination = do
  dsts <- fmap (zip [1..]) enumerateDestinations
  when (null dsts) $ do
    putStrLn "Couldn't find any MIDI destinations."
    exitFailure
  putStrLn "Destinations:"
  forM_ dsts $ \(i, dst) -> do
    name <- getName dst
    putStrLn $ show i ++ ") " ++ name
  promptInt "Which destination?" dsts

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
      putStrLn "Running. Ctrl+C to quit."
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
