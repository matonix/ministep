module Data.DDR where

import           Data.Ratio
import           Data.SSC.Types
import           RIO
import qualified RIO.Vector                    as V

type DDR = Vector Note

data Note = Note
  { absBeat :: AbsBeat
  , event :: Maybe Event
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

fromSSC :: SSC -> Vector DDR
fromSSC = V.map notesToDDR . V.mapMaybe notes . body
 where
  notesToDDR :: Notes -> DDR
  notesToDDR = consAbsBeat . V.map measureToEvents
   where
    consAbsBeat :: Vector (Vector (Maybe Event)) -> DDR
    consAbsBeat vvec = join . flip V.imap vvec $ \measure' vec' ->
      let denominator' = length vec'
      in  flip V.imap vec' $ \numerator' event' ->
            Note (AbsBeat measure' denominator' numerator') event'

  -- Now we ignore BPM change and stop, we see arrows only
  measureToEvents :: Measure -> Vector (Maybe Event)
  measureToEvents = V.map $ fmap arrowToEvent . fromBeatColumn
   where
    arrowToEvent a = Event (Just a) Nothing Nothing

  fromBeatColumn :: BeatColumn -> Maybe Arrow
  fromBeatColumn bc
    | V.length bc == 4 = maybeArrow $ Arrow
      { left  = fromNoteValue =<< bc V.!? 0
      , down  = fromNoteValue =<< bc V.!? 1
      , up    = fromNoteValue =<< bc V.!? 2
      , right = fromNoteValue =<< bc V.!? 3
      }
    | otherwise = Nothing
   where
    maybeArrow (Arrow Nothing Nothing Nothing Nothing) = Nothing
    maybeArrow arr = Just arr

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

prettyPrint :: DDR -> String
prettyPrint = unlines . V.toList . V.map ppNote
  where
    ppNote (Note (AbsBeat m d n) e) = concat
      [ show m 
      , "\t"
      , show d
      , "\t"
      , show n
      , "\t"
      , "|"
      , maybe " " (ppNoteType "<") . join . fmap left . join $ fmap arrow e
      , maybe " " (ppNoteType "v") . join . fmap down . join $ fmap arrow e
      , maybe " " (ppNoteType "^") . join . fmap up . join $ fmap arrow e
      , maybe " " (ppNoteType ">") . join . fmap right . join $ fmap arrow e
      , "|"
      , maybe "" ((\b -> "BPM=" ++ b ++ ", ") . show) . join $ fmap changeBPM e
      , maybe "" ((\s -> "stop=" ++ s ++ ", ") . show) . join $ fmap stop e
      ]
      where
        ppNoteType arr Normal = arr
        ppNoteType _ Freeze = "f"
        ppNoteType _ Release = "r"
        ppNoteType _ Shock = "x"
