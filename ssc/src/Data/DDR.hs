{-# LANGUAGE OverloadedStrings #-}

module Data.DDR
  ( fromSSC,
    reverseDDR,
    prettyPrint,
  )
where

import Data.DDR.Types
import qualified Data.SSC.Types as S
import RIO hiding (Arrow)
import qualified RIO.Map as M
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V'
import qualified Data.Ratio as R

type EventTL = (Map Rational BPM, Map Rational Second)

fromSSC :: S.SSC -> Vector Notes
fromSSC ssc = V.mapMaybe constNotes $ S.body ssc
  where
    bpms = maybe M.empty fromTiming . S.bpms $ S.header ssc
    stops = maybe M.empty fromTiming . S.bpms $ S.header ssc
    events = (bpms, stops)

    constNotes noteData = do
      mode' <- fromStepstype <$> S.stepstype noteData
      rep' <- Just Arrow
      notes' <- fromNotes events <$> S.notes noteData
      return $ Notes mode' rep' notes'

fromTiming :: S.Timing a -> Map Rational a
fromTiming = M.fromList . V.toList . V.map (first $ \x -> R.approxRational x 0.001)

fromStepstype :: Text -> Mode
fromStepstype "dance-single" = Single
fromStepstype "dance-double" = Double
fromStepstype _ = error "no parse stepstype"

fromNotes :: EventTL -> S.Notes -> Vector Note
fromNotes events = attachEvents events . consAbsBeat . V.map measureToEvents
  where
    consAbsBeat :: Vector (Vector Event) -> Vector Note
    consAbsBeat vvec = join . flip V.imap vvec $ \measure' vec' ->
      let denominator' = length vec'
       in flip V.imap vec' $ \numerator' event' ->
            Note (AbsBeat measure' denominator' numerator') event'
    attachEvents :: EventTL -> Vector Note -> Vector Note
    attachEvents events' = V.map $ \(Note a e) -> Note a (updateEvent a e events')
      where
        updateEvent a e (bpms, stops) = 
          e { changeBPM = bpms M.!? toFourBeat a
            , stop = stops M.!? toFourBeat a
            }
        toFourBeat :: AbsBeat -> Rational
        toFourBeat (AbsBeat m d n) = (fromIntegral m + fromIntegral n R.% fromIntegral d) * 4

measureToEvents :: S.Measure -> Vector Event
measureToEvents = V.map $ arrowToEvent . beatColumnToPanel
  where
    -- Now it ignore BPM change and stop, it add arrows only
    -- The other events are added by attachEvents
    arrowToEvent a = ArrowEvent a Nothing Nothing

beatColumnToPanel :: S.BeatColumn -> Panel NoteType
beatColumnToPanel = panelFromVector . V.map fromNoteValue

panelFromVector :: Vector (Maybe a) -> Panel a
panelFromVector vec
  | V.length vec == 4 =
    SinglePanel
      (vec V'.! 0)
      (vec V'.! 1)
      (vec V'.! 2)
      (vec V'.! 3)
  | V.length vec == 8 =
    DoublePanel
      (vec V'.! 0)
      (vec V'.! 1)
      (vec V'.! 2)
      (vec V'.! 3)
      (vec V'.! 4)
      (vec V'.! 5)
      (vec V'.! 6)
      (vec V'.! 7)
  | otherwise = error "invalid panel size"

fromNoteValue :: S.NoteValue -> Maybe NoteType
fromNoteValue S.None = Nothing
fromNoteValue S.Tap = Just Normal
fromNoteValue S.HoldHead = Just Freeze
fromNoteValue S.Tail = Just Release
fromNoteValue S.RollHead = Nothing
fromNoteValue S.Mine = Just Shock
fromNoteValue S.AutoKey = Nothing
fromNoteValue S.Lift = Nothing
fromNoteValue S.Fake = Nothing

-- # Modifier

reverseDDR :: Notes -> Notes
reverseDDR (Notes m re n) = Notes m re (V.map reverse' n)
  where
    reverse' (Note a (ArrowEvent p c s)) = Note a (ArrowEvent (rev p) c s)
    reverse' (Note a (FootEvent f)) = Note a (FootEvent (rev f))
    rev (SinglePanel l d u r) = SinglePanel r u d l
    rev (DoublePanel l1 d1 u1 r1 l2 d2 u2 r2) =
      DoublePanel r2 u2 d2 l2 r1 u1 d1 l1

-- # Pretty Printing

prettyPrint :: Notes -> String
prettyPrint = unlines . V.toList . V.map ppNote . notes
  where
    ppNote (Note (AbsBeat m d n) e) =
      concat
        [ show m,
          "\t",
          show d,
          "\t",
          show n,
          "\t",
          "|",
          maybe " " (ppNoteType "<") $ up $ panel e,
          maybe " " (ppNoteType "v") $ down $ panel e,
          maybe " " (ppNoteType "^") $ up $ panel e,
          maybe " " (ppNoteType ">") $ right $ panel e,
          "|",
          maybe "" (\b -> "BPM=" ++ show b ++ ", ") $ changeBPM e,
          maybe "" (\s -> "stop=" ++ show s ++ ", ") $ stop e
        ]
      where
        ppNoteType arr Normal = arr
        ppNoteType _ Freeze = "f"
        ppNoteType _ Release = "r"
        ppNoteType _ Shock = "x"
