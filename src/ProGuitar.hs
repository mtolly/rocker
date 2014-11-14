module ProGuitar where

import Data.Word (Word8)
import Data.Bits ((.|.), (.&.))
import Control.Monad (guard)

data Controller = Mustang | Squier
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Fret = Int
type Velocity = Int

-- | 'S6' is low E, 'S1' is high E
data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Command = (Controller, Message)

data Message
  = Fret GtrString Fret
  | Strum GtrString Velocity
  | KeepAlive
  | ChangeButtons Buttons
  deriving (Eq, Ord, Show, Read)

data Buttons = Buttons
  { buttonX :: Bool
  , buttonA :: Bool
  , buttonB :: Bool
  , buttonY :: Bool
  , buttonBack :: Bool
  , buttonStart :: Bool
  , buttonXbox :: Bool
  , buttonSync :: Bool
  , buttonDpad :: Dpad
  , button32 :: Bool -- ^ Unknown
  , buttonTilt :: Bool
  } deriving (Eq, Ord, Show, Read)

data Dpad
  = Up
  | UpRight
  | Right
  | DownRight
  | Down
  | DownLeft
  | Left
  | UpLeft
  | Center
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

basePitch :: (Num a) => GtrString -> a
basePitch s = case s of
  S6 -> 40
  S5 -> 45
  S4 -> 50
  S3 -> 55
  S2 -> 59
  S1 -> 64

strNumber :: (Num a) => GtrString -> a
strNumber s = case s of
  S6 -> 6
  S5 -> 5
  S4 -> 4
  S3 -> 3
  S2 -> 2
  S1 -> 1

sendCommand :: Command -> [Word8]
sendCommand (cont, msg) = let
  magic = case cont of
    Mustang -> [8, 64, 10]
    Squier  -> [8, 64, 8 ]
  in magic ++ case msg of
    Fret str frt -> [1, strNumber str, fromIntegral $ basePitch str + frt]
    Strum str vel -> [5, strNumber str, fromIntegral vel]
    KeepAlive -> 9 : replicate 12 0
    ChangeButtons btns -> let flag n f = if f btns then n else 0 in
      [ foldr (.|.) 0
        [ flag 1 buttonX
        , flag 2 buttonA
        , flag 4 buttonB
        , flag 8 buttonY
        ]
      , foldr (.|.) 0
        [ flag 1 buttonBack
        , flag 2 buttonStart
        , flag 16 buttonXbox
        , flag 64 buttonSync
        ]
      , foldr (.|.) 0
        [ fromIntegral $ fromEnum $ buttonDpad btns
        , flag 32 button32
        , flag 64 buttonTilt
        ]
      , 0
      ]

-- | If @a@ is a \"subtype\" of @b@, given a function to generalize @a@ to @b@,
-- determines if a given @b@ is the generalization of some @a@.
lookupTable :: (Eq b, Enum a, Bounded a) => (a -> b) -> b -> Maybe a
lookupTable f y = lookup y $ map (\x -> (f x, x)) [minBound .. maxBound]

receiveCommand :: [Word8] -> Maybe Command
receiveCommand = magic where
  magic (8 : 64 : 10 : xs) = fmap ((,) Mustang) $ msg xs
  magic (8 : 64 : 8  : xs) = fmap ((,) Squier ) $ msg xs
  magic _ = Nothing
  msg [1, nstr, pitch] = do
    str <- lookupTable strNumber nstr
    let fret = fromIntegral pitch - basePitch str
    guard $ 0 <= fret && fret <= 22
    return $ Fret str fret
  msg [5, nstr, vel] = do
    str <- lookupTable strNumber nstr
    guard $ 1 <= vel && vel <= 127
    return $ Strum str $ fromIntegral vel
  msg (9 : rest) = do
    guard $ rest == replicate 12 0
    return KeepAlive
  msg [8, b1, b2, b3] = do
    dpad <- lookupTable (fromIntegral . fromEnum) $ b1 .&. 15
    return $ ChangeButtons $ Buttons
      { buttonX = test 1 b1
      , buttonA = test 2 b1
      , buttonB = test 4 b1
      , buttonY = test 8 b1
      , buttonBack = test 1 b2
      , buttonStart = test 2 b2
      , buttonXbox = test 16 b2
      , buttonSync = test 64 b2
      , buttonDpad = dpad
      , button32 = test 32 b3
      , buttonTilt = test 64 b3
      }
    where test n b = (n .&. b) /= 0
  msg _ = Nothing
