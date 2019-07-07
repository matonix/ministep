module Data.DDR where

import           Data.Ratio
import           Data.SSC.Types
import qualified RIO.Vector                    as V
import           RIO

type Note = (AbsBeat, Event)
type DDR = [Note]

-- If a measure has a 8th beat at last of the measure, 
-- denominator = 8 and numerater = 7
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
  { arrow :: Maybe Arrow
  , changeBPM :: Maybe Float
  , stop :: Maybe MilliSecond
  } deriving Show

type MilliSecond = Float

data Arrow = Arrow
  { left :: Maybe NoteType
  , down :: Maybe NoteType
  , up :: Maybe NoteType
  , right :: Maybe NoteType
  } deriving Show

data NoteType = Normal | Freeze | Release | Shock
  deriving Show

fromSSC :: SSC -> Vector (Maybe DDR)
fromSSC = V.map notesToDDR . V.mapMaybe notes . body

notesToDDR :: Notes -> Maybe DDR
notesToDDR = undefined

measureToArrows :: Measure -> Maybe [Event]
measureToArrows = undefined



fromBeatColumn :: BeatColumn -> Maybe Arrow
fromBeatColumn bc
  | V.length bc == 4 = Just $ Arrow { left  = fromNoteValue =<< bc V.!? 0
                                    , down  = fromNoteValue =<< bc V.!? 1
                                    , up    = fromNoteValue =<< bc V.!? 2
                                    , right = fromNoteValue =<< bc V.!? 3
                                    }
  | otherwise = Nothing

fromNoteValue :: NoteValue -> Maybe NoteType
fromNoteValue None     = Nothing
fromNoteValue Tap      = Just Normal
fromNoteValue HoldHead = Just Freeze
fromNoteValue Tail     = Just Release
fromNoteValue RollHead = Nothing
fromNoteValue Mine     = Just Shock
fromNoteValue AutoKey  = Nothing
fromNoteValue Lift     = Nothing
fromNoteValue Fake     = Nothing
