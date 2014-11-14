module Main (main) where

import System.MIDI
import Control.Monad
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Text.Read (readMaybe)
import System.Exit (exitFailure)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import qualified Drums as D
import qualified ProGuitar as PG

withSource :: Source -> Maybe ClientCallback -> (Connection -> IO a) -> IO a
withSource src cc = bracket (openSource src cc) close

withDestination :: Destination -> (Connection -> IO a) -> IO a
withDestination dst = bracket (openDestination dst) close

-- | Display a dialog which asks the user to pick a number corresponding to
-- a choice from a list.
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

-- | Prompt the user to pick a MIDI source.
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

-- | Prompt the user to pick a MIDI destination.
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

-- | A layout for playing drums with a keyboard. Uses symmetrical layouts for
-- the left and right hands, with white keys as drums and black as cymbals,
-- and also uses the sustain pedal as the kick drum.
onyxDrums :: MidiMessage -> Maybe D.Command
onyxDrums mm = let
  rightDrums = [D.Kick, D.CymY, D.Snare, D.TomY, D.CymB, D.TomB, D.CymG, D.TomG]
  rightHand = zip [45 ..] rightDrums
  leftHand = zip [43, 42 ..] rightDrums
  in case mm of
    MidiMessage _ (NoteOn k _) -> lookup k $ rightHand ++ leftHand
    MidiMessage _ (CC 64 127)  -> Just D.Kick
    _                          -> Nothing

onyxDrums' :: MidiMessage -> StateT s IO [MidiMessage]
onyxDrums' mm = return $ case onyxDrums mm of
  Nothing  -> []
  Just mm' -> [MidiMessage 1 $ D.sendCommand mm']

viewPG :: MidiMessage -> StateT s IO [MidiMessage]
viewPG mm = lift $ do
  print mm
  case mm of
    SysEx bytes -> case PG.receiveCommand bytes of
      Just cmd -> print cmd
      Nothing -> return ()
    _ -> return ()
  return []

translate :: s -> (MidiMessage -> StateT s IO [MidiMessage]) -> IO ()
translate initState f = do
  src <- getSource
  dst <- getDestination
  withSource src Nothing $ \scon ->
    withDestination dst $ \dcon -> do
      start scon
      putStrLn "Running. Ctrl+C to quit."
      flip evalStateT initState $ forever $ do
        evt <- lift $ getNextEvent scon
        case evt of
          Just (MidiEvent _ mm) -> f mm >>= mapM_ (lift . send dcon)
          Nothing -> lift $ threadDelay 100

main :: IO ()
main = translate () onyxDrums'
