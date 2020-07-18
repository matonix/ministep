{-# LANGUAGE OverloadedStrings #-}
module Data.DDR where

import           Data.Ratio
import qualified Data.SSC.Types                as S
import           RIO                     hiding ( Arrow )
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V'

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
  { arrow :: Panel
  , changeBPM :: Maybe Float
  , stop :: Maybe MilliSecond
  } deriving Show

type MilliSecond = Float

data Panel = SinglePanel
  { left :: Maybe NoteType
  , down :: Maybe NoteType
  , up :: Maybe NoteType
  , right :: Maybe NoteType
  } | DoublePanel
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

-- Transform Function

fromSSC :: S.SSC -> Vector Notes
fromSSC = V.mapMaybe constNotes . S.body
 where
  constNotes noteData = do
    mode' <- fmap fromStepstype $ S.stepstype noteData
    rep' <- Just Arrow
    notes' <- fmap fromNotes $ S.notes noteData
    return $ Notes mode' rep' notes'

  fromStepstype :: Text -> Mode
  fromStepstype "dance-single" = Single
  fromStepstype "dance-double" = Double
  fromStepstype _ = error "no parse stepstype"

  fromNotes :: S.Notes -> Vector Note
  fromNotes = consAbsBeat . V.map measureToEvents
   where
    consAbsBeat :: Vector (Vector Event) -> Vector Note
    consAbsBeat vvec = join . flip V.imap vvec $ \measure' vec' ->
      let denominator' = length vec'
      in  flip V.imap vec' $ \numerator' event' ->
            Note (AbsBeat measure' denominator' numerator') event'

  measureToEvents :: S.Measure -> Vector Event
  measureToEvents = V.map $ arrowToEvent . beatColumnToPanel
    where
      -- Now we ignore BPM change and stop, we see arrows only
      arrowToEvent a = Event a Nothing Nothing

  beatColumnToPanel :: S.BeatColumn -> Panel
  beatColumnToPanel =
    panelFromVector . V.map fromNoteValue

  panelFromVector :: Vector (Maybe NoteType) -> Panel
  panelFromVector vec
    | V.length vec == 4 = SinglePanel (vec V'.! 0) (vec V'.! 1) (vec V'.! 2) (vec V'.! 3)
    | otherwise = error "invalid panel size"
      
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

prettyPrint :: Notes -> String
prettyPrint = unlines . V.toList . V.map ppNote . notes
 where
  ppNote (Note (AbsBeat m d n) e) = concat
    [ show m
    , "\t"
    , show d
    , "\t"
    , show n
    , "\t"
    , "|"
    , maybe " " (ppNoteType "<") $ up $ arrow e
    , maybe " " (ppNoteType "v") $ down $ arrow e
    , maybe " " (ppNoteType "^") $ up $ arrow e
    , maybe " " (ppNoteType ">") $ right $ arrow e
    , "|"
    , maybe "" (\b -> "BPM=" ++ show b ++ ", ") $ changeBPM e
    , maybe "" (\s -> "stop=" ++ show s ++ ", ") $ stop e
    ]
   where
    ppNoteType arr Normal  = arr
    ppNoteType _   Freeze  = "f"
    ppNoteType _   Release = "r"
    ppNoteType _   Shock   = "x"
