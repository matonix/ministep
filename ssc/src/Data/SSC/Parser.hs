{-# LANGUAGE OverloadedStrings #-}

-- SSC loader: https://github.com/stepmania/stepmania/blob/27fdfb38718474253aeb33e9f9c7fd1a91ed823a/src/NotesLoaderSSC.cpp 
-- SM loader: https://github.com/stepmania/stepmania/blob/27fdfb38718474253aeb33e9f9c7fd1a91ed823a/src/NotesLoaderSM.cpp

module Data.SSC.Parser where

import           RIO                     hiding ( many
                                                , some
                                                )
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as H
import qualified RIO.List                      as List
import           Text.Megaparsec
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.List.Extra                ( split )
import           Data.SSC.Types
import           Data.Scientific

type Parser = Parsec SSCError Text

type SSCError = Void

type Tag = (Text, Text) -- (Label, Content)

decodeHelper :: Text -> Either (ParseErrorBundle Text SSCError) SSC
decodeHelper input = runParser parseSSC "" input

decodeHelperWithFile :: FilePath -> Either (ParseErrorBundle Text SSCError) SSC
decodeHelperWithFile path = runParser parseSSC path mempty

parseSSC :: Parser SSC
parseSSC = between sc eof parseTags

parseTags :: Parser SSC
parseTags = do
  tags <- some parseTag
  if any isNoteData tags
    then do
      let (t : ts) = split isNoteData tags
      SSC <$> parseHeader t <*> parseBodySSC ts
    else do
      let (t, ts) = List.span (not . isNotes) tags
      SSC <$> parseHeader t <*> parseBodySM ts
 where
  isNoteData = (== "NOTEDATA") . fst
  isNotes    = (== "NOTES") . fst

parseHeader :: [Tag] -> Parser Header
parseHeader tags =
  let tags' = H.fromList tags
  in  return $ Header
        (parseValue tags' "VERSION" parseText)
        (parseValue tags' "TITLE" parseText)
        (parseValue tags' "SUBTITLE" parseText)
        (parseValue tags' "ARTIST" parseText)
        (parseValue tags' "TITLETRANSLIT" parseText)
        (parseValue tags' "SUBTITLETRANSLIT" parseText)
        (parseValue tags' "ARTISTTRANSLIT" parseText)
        (parseValue tags' "GENRE" parseText)
        (parseValue tags' "ORIGIN" parseText)
        (parseValue tags' "CREDIT" parseText)
        (parseValue tags' "BANNER" parseFilePath)
        (parseValue tags' "BACKGROUND" parseFilePath)
        (parseValue tags' "PREVIEWVID" parseFilePath)
        (parseValue tags' "JACKET" parseFilePath)
        (parseValue tags' "CDIMAGE" parseFilePath)
        (parseValue tags' "DISCIMAGE" parseFilePath)
        (parseValue tags' "LYRICSPATH" parseFilePath)
        (parseValue tags' "CDTITLE" parseFilePath)
        (parseValue tags' "MUSIC" parseFilePath)
        (parseValue tags' "PREVIEW" parseFilePath)
        (parseValue tags' "OFFSET" parseBeat)
        (parseValue tags' "SAMPLESTART" parseSec)
        (parseValue tags' "SAMPLELENGTH" parseSec)
        (parseValue tags' "SELECTABLE" parseSelectable)
        (parseValue tags' "BPMS" parseTimingBPM)
        (parseValue tags' "DISPLAYBPM" parseDisplayBPM)
        (parseValue tags' "STOPS" parseTimingSec)
        (parseValue tags' "DELAYS" parseTimingSec)
        (parseValue tags' "WARPS" parseTimingSec)
        (parseValue tags' "TIMESIGNATURES" parseTimingTimeSignatures)
        (parseValue tags' "TICKCOUNTS" parseTimingRate)
        (parseValue tags' "COMBOS" parseUnknown)
        (parseValue tags' "SPEEDS" parseUnknown)
        (parseValue tags' "SCROLLS" parseUnknown)
        (parseValue tags' "FAKES" parseUnknown)
        (parseValue tags' "LABELS" parseUnknown) -- Timing Text
        (parseValue tags' "BGCHANGES" parseUnknown) -- Timing BGChanges
        (parseValue tags' "KEYSOUNDS" parseUnknown)
        (parseValue tags' "ATTACKS" parseUnknown)

parseBodySSC :: [[Tag]] -> Parser Body
parseBodySSC tags = V.fromList <$> mapM parseSSCStyle tags

parseBodySM :: [Tag] -> Parser Body
parseBodySM tags = V.fromList <$> mapM parseSMStyle tags

parseSSCStyle :: [Tag] -> Parser NoteData
parseSSCStyle tags =
  let tags' = H.fromList tags
  in  return $ SSCStyle (parseValue tags' "CHARTNAME" parseText)
                        (parseValue tags' "STEPSTYPE" parseText)
                        (parseValue tags' "DESCRIPTION" parseText)
                        (parseValue tags' "CHARTSTYLE" parseText)
                        (parseValue tags' "DIFFICULTY" parseText)
                        (parseValue tags' "METER" parseMeter)
                        (parseValue tags' "RADARVALUES" parseVectorFloat)
                        (parseValue tags' "CREDIT" parseText)
                        (parseValue tags' "NOTES" parseNotes)

parseSMStyle :: Tag -> Parser NoteData
parseSMStyle tag = return $ fromMaybe def $ parseMaybe p $ snd tag
 where
  def = SMStyle Nothing Nothing Nothing Nothing Nothing Nothing
  p =
    SMStyle
      <$> parseValueSM (parseTextBut ':') <* symbol ":"
      <*> parseValueSM (parseTextBut ':') <* symbol ":"
      <*> parseValueSM (parseTextBut ':') <* symbol ":"
      <*> parseValueSM parseMeter <* symbol ":"
      <*> parseValueSM parseVectorFloat <* symbol ":"
      <*> parseValueSM parseNotes
-- parseSMStyle value = case T.split (== ':') $ snd value of
--   [stepstype', credit', difficulty', meter', radarvalues', notes'] ->
--     return $ SMStyle (parseValueSM (parseTextBut ':') stepstype')
--                      (parseValueSM (parseTextBut ':') credit')
--                      (parseValueSM (parseTextBut ':') difficulty')
--                      (parseValueSM parseMeter meter')
--                      (parseValueSM parseVectorFloat radarvalues')
--                      (parseValueSM parseNotes notes')
--   _ -> return $ SMStyle Nothing Nothing Nothing Nothing Nothing Nothing

parseTag :: Parser Tag
parseTag = (,) <$ symbol "#" <*> key <*> value
 where
  key   = T.toUpper . T.pack <$> someTill C.letterChar (symbol ":")
  value = T.pack <$> manyTill (anySingleBut ';') (symbol ";")

parseValue :: HashMap Text Text -> Text -> Parser a -> Maybe a
parseValue tags text parser = case H.lookup text tags of
  Nothing    -> Nothing
  Just value -> parseMaybe parser value

parseValueSM :: Parser a -> Parser (Maybe a)
parseValueSM parser = Just <$ many C.spaceChar <*> parser

-- | Specific Parsers

parseText :: Parser Text
parseText = T.pack <$> many anySingle

parseTextBut :: Char -> Parser Text
parseTextBut t = T.pack <$> many (anySingleBut t)

parseFilePath :: Parser FilePath
parseFilePath = many C.printChar

parseBeat :: Parser Beat
parseBeat = parseFloat

parseSec :: Parser Sec
parseSec = parseFloat

parseSelectable :: Parser Selectable
parseSelectable = Yes <$ C.string' "yes" <|> No <$ C.string' "no"

parseTimingBPM :: Parser (Timing BPM)
parseTimingBPM = parseTiming parseFloat

parseDisplayBPM :: Parser DisplayBPM
parseDisplayBPM =
  Shuffle
    <$  symbol "*"
    <|> (Single <$> parseFloat)
    <|> (Range <$> parseFloat <*> parseFloat)

parseTimingSec :: Parser (Timing Sec)
parseTimingSec = parseTiming parseFloat

parseTimingTimeSignatures :: Parser (Timing TimeSignatures)
parseTimingTimeSignatures = parseTiming parseTimeSignatures
  where parseTimeSignatures = TimeSignatures <$> L.decimal <*> L.decimal

parseTimingRate :: Parser (Timing Rate)
parseTimingRate = parseTiming parseFloat

parseUnknown :: Parser Unknown
parseUnknown = parseText

parseMeter :: Parser Meter
parseMeter = L.decimal

parseVectorFloat :: Parser (Vector Float)
parseVectorFloat = parseVector parseFloat

parseNotes :: Parser Notes
parseNotes = V.fromList <$> sepBy parseMeasure (symbol ",")

parseMeasure :: Parser Measure
parseMeasure = V.fromList <$> sepEndBy parseBeatColumn C.crlf

parseBeatColumn :: Parser BeatColumn
parseBeatColumn = V.fromList <$> some parseNoteValue

parseNoteValue :: Parser NoteValue
parseNoteValue =
  None
    <$  single '0'
    <|> Tap
    <$  single '1'
    <|> HoldHead
    <$  single '2'
    <|> Tail
    <$  single '3'
    <|> RollHead
    <$  single '4'
    <|> Mine
    <$  single 'M'
    <|> AutoKey
    <$  single 'K'
    <|> Lift
    <$  single 'L'
    <|> Fake
    <$  single 'F'

-- | Combinators

sc :: Parser ()
sc = L.space C.space1 lineCmnt empty where lineCmnt = L.skipLineComment "//"

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseTiming :: Parser a -> Parser (Timing a)
parseTiming parser = V.fromList <$> sepBy pair (symbol ",")
  where pair = (,) <$> parseBeat <* symbol "=" <*> parser

parseVector :: Parser a -> Parser (Vector a)
parseVector parser = V.fromList <$> sepBy parser (symbol ",")

parseFloat :: Parser Float
parseFloat =
  toRealFloat <$> L.signed sc L.scientific <|> toRealFloat <$> L.scientific
