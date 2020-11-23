module Data.DDR.Types where

import Data.Ratio
import RIO

data Notes = Notes
  { mode :: Mode,
    rep :: Rep,
    notes :: Vector Note
  }
  deriving (Show)

data Mode = Single | Double deriving (Show)

data Rep = Arrow | Foot deriving (Show)

data Note = Note
  { absBeat :: AbsBeat,
    event :: Event
  }
  deriving (Show)

-- If a measure has a 8th beat at last of the measure,
-- denominator = 8 and numerater = 7 (0-indexed)
data AbsBeat = AbsBeat
  { measure :: Int,
    denominator :: Int,
    numerator :: Int
  }
  deriving (Show, Eq)

instance Ord AbsBeat where
  AbsBeat m1 d1 n1 `compare` AbsBeat m2 d2 n2 = case m1 `compare` m2 of
    LT -> LT
    GT -> GT
    EQ -> (n1 % d1) `compare` (n2 % d2)

data Event
  = ArrowEvent
      { panel :: Panel NoteType,
        changeBPM :: Maybe BPM,
        stop :: Maybe Second
      }
  | FootEvent
      { foot :: Panel Foot
      }
  deriving (Show)

type BPM = Float

type Second = Float

data Panel a
  = SinglePanel
      { left :: Maybe a,
        down :: Maybe a,
        up :: Maybe a,
        right :: Maybe a
      }
  | DoublePanel
      { left1p :: Maybe a,
        down1p :: Maybe a,
        up1p :: Maybe a,
        right1p :: Maybe a,
        left2p :: Maybe a,
        down2p :: Maybe a,
        up2p :: Maybe a,
        right2p :: Maybe a
      }
  deriving (Show)

data NoteType = Normal | Freeze | Release | Shock
  deriving (Show)

data Foot = FL | FR
  deriving (Show)
