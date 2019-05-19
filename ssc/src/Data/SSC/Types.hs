{-# LANGUAGE DuplicateRecordFields #-}

module Data.SSC.Types where

import           RIO

type Body = Vector NoteData
data SSC = SSC Header Body
  deriving Show

type Beat = Float
type Sec = Float
type BPM = Float
type Rate = Float
type Binary = Int
type Meter = Int

data Selectable = Yes | No
  deriving Show
data DisplayBPM = Shuffle | Single Float | Range Float Float
  deriving Show
type Timing a = Vector (Beat, a)
-- type BGChanges = (FilePath, Binary, Binary, Binary)
data TimeSignatures = TimeSignatures -- | numerator / denominator
  { numerator :: Int
  , denominator :: Int 
  } deriving Show
type Unknown = Text

data Header = Header
  { version :: Maybe Text
  , title :: Maybe Text
  , subtitle :: Maybe Text
  , artist :: Maybe Text
  , titletranslit :: Maybe Text
  , subtitletranslit :: Maybe Text
  , artisttranslit :: Maybe Text
  , genre :: Maybe Text
  , origin :: Maybe Text
  , credit :: Maybe Text
  , banner :: Maybe FilePath
  , background :: Maybe FilePath
  , previewvid :: Maybe FilePath
  , jacket :: Maybe FilePath
  , cdimage :: Maybe FilePath
  , discimage :: Maybe FilePath
  , lyricspath :: Maybe FilePath
  , cdtitle :: Maybe FilePath
  , music :: Maybe FilePath
  , preview :: Maybe FilePath
  , offset :: Maybe Beat
  , samplestart :: Maybe Sec
  , samplelength :: Maybe Sec
  , selectable :: Maybe Selectable
  , bpms :: Maybe (Timing BPM)
  , displaybpm :: Maybe DisplayBPM
  , stops :: Maybe (Timing Sec)
  , delays :: Maybe (Timing Sec)
  , warps :: Maybe (Timing Sec)
  , timesignatures :: Maybe (Timing TimeSignatures)
  , tickcounts :: Maybe (Timing Rate)
  , combos :: Maybe Unknown
  , speeds :: Maybe Unknown
  , scrolls :: Maybe Unknown
  , fakes :: Maybe Unknown
  , labels :: Maybe Unknown -- Timing Text
  , bgchanges :: Maybe Unknown -- Timing BGChanges
  , keysounds :: Maybe Unknown
  , attacks :: Maybe Unknown
  } deriving Show

data NoteData = NoteData
  { chartname :: Maybe Text
  , stepstype :: Maybe Text
  , description :: Maybe Text
  , chartstyle :: Maybe Text
  , difficulty :: Maybe Text
  , meter :: Maybe Meter
  , radarvalues :: Maybe (Vector Float)
  , credit :: Maybe Text
  , notes :: Maybe Notes
  } deriving Show
  -- | SMStyle
  -- { stepstype :: Text
  -- , credit :: Text
  -- , difficulty :: Text
  -- , meter :: Int
  -- , radarvalues :: Vector Float
  -- , notes :: Notes
  -- }

type Notes = Vector Measure

type Measure = Vector BeatColumn

type BeatColumn = Vector NoteValue

-- data Note = Note Key Measure NoteValues TimingSegments

-- data Key = Left | Down | Up | Right

-- data Measure = D4 | D8 | D12 | D16 | D24 | D32 | D48 | D64 | D192

type NoteValues = [NoteValue]

data NoteValue = None | Tap | HoldHead | Tail | RollHead | Mine | AutoKey | Lift | Fake
  deriving Show

-- data TimingSegments = BPM | Stop | Delay
