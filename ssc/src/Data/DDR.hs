{-# LANGUAGE OverloadedStrings #-}
module Data.DDR
  ( fromSSC
  , prettyPrint
  )
where

import           RIO                     hiding ( Arrow )
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as V'

import qualified Data.SSC.Types                as S
import           Data.DDR.Types

fromSSC :: S.SSC -> Vector Notes
fromSSC = V.mapMaybe constNotes . S.body
 where
  constNotes noteData = do
    mode'  <- fmap fromStepstype $ S.stepstype noteData
    rep'   <- Just Arrow
    notes' <- fmap fromNotes $ S.notes noteData
    return $ Notes mode' rep' notes'

  fromStepstype :: Text -> Mode
  fromStepstype "dance-single" = Single
  fromStepstype "dance-double" = Double
  fromStepstype _              = error "no parse stepstype"

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
  beatColumnToPanel = panelFromVector . V.map fromNoteValue

  panelFromVector :: Vector (Maybe NoteType) -> Panel
  panelFromVector vec
    | V.length vec == 4 = SinglePanel (vec V'.! 0)
                                      (vec V'.! 1)
                                      (vec V'.! 2)
                                      (vec V'.! 3)
    | V.length vec == 8 = DoublePanel (vec V'.! 0)
                                      (vec V'.! 1)
                                      (vec V'.! 2)
                                      (vec V'.! 3)
                                      (vec V'.! 4)
                                      (vec V'.! 5)
                                      (vec V'.! 6)
                                      (vec V'.! 7)
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
    , maybe " " (ppNoteType "<") $ up $ panel e
    , maybe " " (ppNoteType "v") $ down $ panel e
    , maybe " " (ppNoteType "^") $ up $ panel e
    , maybe " " (ppNoteType ">") $ right $ panel e
    , "|"
    , maybe "" (\b -> "BPM=" ++ show b ++ ", ") $ changeBPM e
    , maybe "" (\s -> "stop=" ++ show s ++ ", ") $ stop e
    ]
   where
    ppNoteType arr Normal  = arr
    ppNoteType _   Freeze  = "f"
    ppNoteType _   Release = "r"
    ppNoteType _   Shock   = "x"
