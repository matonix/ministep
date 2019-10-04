module Data.DDR where

import           Data.Ratio
import qualified Data.SSC.Types                as S
import           RIO
import qualified RIO.Vector                    as V

type DDR a = Vector (Note a)

data Note a = Note
  { absBeat :: AbsBeat
  , event :: Maybe (Event a)
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

data Event a = Event
  { arrow :: Maybe (Panel a)
  , changeBPM :: Maybe Float
  , stop :: Maybe MilliSecond
  } deriving Show

type MilliSecond = Float

data Panel a = Single
  { left :: a
  , down :: a
  , up :: a
  , right :: a
  } deriving Show

type Arrow = Maybe NoteType

data NoteType = Normal | Freeze | Release | Shock
  deriving Show

-- Helper

panelFromList :: [a] -> Maybe (Panel a)
panelFromList [l, d, u, r] = Just $ Single l d u r
panelFromList _            = Nothing

panelToList :: Panel a -> [a]
panelToList (Single l d u r) = [l, d, u, r]

panelFromVector :: Vector a -> Maybe (Panel a)
panelFromVector = panelFromList . V.toList

-- Transform Function

fromSSC :: S.SSC -> Vector (DDR Arrow)
fromSSC = V.map notesToDDR . V.mapMaybe S.notes . S.body
 where
  notesToDDR :: S.Notes -> DDR Arrow
  notesToDDR = consAbsBeat . V.map measureToEvents
   where
    consAbsBeat :: Vector (Vector (Maybe (Event Arrow))) -> DDR Arrow
    consAbsBeat vvec = join . flip V.imap vvec $ \measure' vec' ->
      let denominator' = length vec'
      in  flip V.imap vec' $ \numerator' event' ->
            Note (AbsBeat measure' denominator' numerator') event'

  -- Now we ignore BPM change and stop, we see arrows only
  measureToEvents :: S.Measure -> Vector (Maybe (Event Arrow))
  measureToEvents = V.map $ fmap arrowToEvent . fromBeatColumn
    where arrowToEvent a = Event (Just a) Nothing Nothing

  fromBeatColumn :: S.BeatColumn -> Maybe (Panel Arrow)
  fromBeatColumn =
    maybe Nothing panelFromVector . anyJustOrNothing . V.map fromNoteValue
   where
    anyJustOrNothing :: Vector Arrow -> Maybe (Vector Arrow)
    anyJustOrNothing v = if V.any isJust v then Just v else Nothing

  fromNoteValue :: S.NoteValue -> Maybe NoteType
  fromNoteValue S.None     = Nothing
  fromNoteValue S.Tap      = Just Normal
  fromNoteValue S.HoldHead = Just Freeze
  fromNoteValue S.Tail     = Just Release
  fromNoteValue S.RollHead = Nothing
  fromNoteValue S.Mine     = Just Shock
  fromNoteValue S.AutoKey  = Nothing
  fromNoteValue S.Lift     = Nothing
  fromNoteValue S.Fake     = Nothing

-- # Pretty Printing 

prettyPrint :: DDR Arrow -> String
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
    , maybe " " (ppNoteType "<") $ up =<< arrow =<< e
    , maybe " " (ppNoteType "v") $ down =<< arrow =<< e
    , maybe " " (ppNoteType "^") $ up =<< arrow =<< e
    , maybe " " (ppNoteType ">") $ right =<< arrow =<< e
    , "|"
    , maybe "" (\b -> "BPM=" ++ show b ++ ", ") $ changeBPM =<< e
    , maybe "" (\s -> "stop=" ++ show s ++ ", ") $ stop =<< e
    ]
   where
    ppNoteType arr Normal  = arr
    ppNoteType _   Freeze  = "f"
    ppNoteType _   Release = "r"
    ppNoteType _   Shock   = "x"
