module Data.DDR.Types where

import           Data.Ratio
import           RIO

data Notes = Notes
  { mode :: Mode
  , rep :: Rep
  , notes :: Vector Note
  } deriving Show

data Mode = Single | Double deriving Show
data Rep = Arrow | Foot deriving Show

data Note = Note
  { absBeat :: AbsBeat
  , event :: Event
  } deriving Show

-- If a measure has a 8th beat at last of the measure, 
-- denominator = 8 and numerater = 7 (0-indexed)
data AbsBeat = AbsBeat
  { measure :: Int
  , denominator :: Int
  , numerator :: Int
  } deriving (Show, Eq)

instance Ord AbsBeat where
  AbsBeat m1 d1 n1 `compare` AbsBeat m2 d2 n2 = case m1 `compare` m2 of
    LT -> LT
    GT -> GT
    EQ -> (n1 % d1) `compare` (n2 % d2)

data Event = Event
  { panel :: Panel
  , changeBPM :: Maybe BPM
  , stop :: Maybe Second
  } deriving Show

type BPM = Float
type Second = Float

data Panel =
  SinglePanel
    { left :: Maybe NoteType
    , down :: Maybe NoteType
    , up :: Maybe NoteType
    , right :: Maybe NoteType
    } 
  | DoublePanel
    { left1p :: Maybe NoteType
    , down1p :: Maybe NoteType
    , up1p :: Maybe NoteType
    , right1p :: Maybe NoteType
    , left2p :: Maybe NoteType
    , down2p :: Maybe NoteType
    , up2p :: Maybe NoteType
    , right2p :: Maybe NoteType
    } deriving Show

data NoteType = Normal | Freeze | Release | Shock
  deriving Show
