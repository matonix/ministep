{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
-- To live view: use vscode SVG Viewer

module Data.SVG.Render where

import           RIO                     hiding ( Arrow )
import qualified RIO.List                      as L
import           Data.SVG.Types
import           Data.Ratio
import           Diagrams.Prelude        hiding ( arrow
                                                , direction
                                                , Direction
                                                , width
                                                )
import           Diagrams.Backend.SVG.CmdLine

type Pos = Point V2 Double
type PosDiagram = (Pos, Diagram B)

data DDRConfig = DDRConfig
  { sparseX :: Rational
  , sparseY :: Rational
  } deriving Show

defaultDDRConfig :: DDRConfig
defaultDDRConfig = DDRConfig 1 8

-- # Diagrams

ddrDiagram :: Objects -> Diagram B
ddrDiagram = position . map (objectDiagram defaultDDRConfig) . L.sortOn Down

objectDiagram :: DDRConfig -> Object -> PosDiagram
objectDiagram conf Note {..} =
  (mkPos conf xPos yPos, arrowDiagram direction noteType yPos)
objectDiagram conf FreezeBar {..} =
  (mkPos conf xPos yPos, freezeBarDiagram conf freezeLength)
objectDiagram conf ShockBar {..} = (mkPos conf xPos yPos, shockBarDiagram conf yPos)
objectDiagram conf ChangeBPM {..} = (mkPos conf (-3) yPos, changeBPMDiagram conf width bpm)
objectDiagram conf Stop {..} = (mkPos conf (-3) yPos, stopDiagram conf width stop)


mkPos :: DDRConfig -> Rational -> Rational -> Pos
mkPos DDRConfig {..} xPos yPos =
  p2 (fromRational (xPos * sparseX), fromRational (-1 * yPos * sparseY))

-- # Arrow Level Diagrams

arrowDiagram :: Direction -> NoteType -> YPos -> Diagram B
arrowDiagram dir Normal  y = arrowBase # rotateArrow dir # colorArrow y
arrowDiagram dir Freeze  y = arrowBase # rotateArrow dir # colorArrow y
arrowDiagram dir Release _ = arrowBase # rotateArrow dir # freezeColorArrow
arrowDiagram dir Shock   _ = arrowBase # rotateArrow dir # shockColorArrow

rotateArrow :: Direction -> Diagram B -> Diagram B
rotateArrow L = rotate (180 @@ deg)
rotateArrow D = rotate (270 @@ deg)
rotateArrow U = rotate (90 @@ deg)
rotateArrow R = rotate (0 @@ deg)

colorArrow :: YPos -> Diagram B -> Diagram B
colorArrow y arr =
  let numer = numerator y
      denom = denominator y
  in  if
        | numer * 4 `mod` denom == 0  -> arr # fc pink # lc darkred
        | numer * 8 `mod` denom == 0  -> arr # fc lightblue # lc indigo
        | numer * 16 `mod` denom == 0 -> arr # fc yellow # lc darkgoldenrod
        | otherwise                   -> arr # fc yellowgreen # lc green

freezeColorArrow :: Diagram B -> Diagram B
freezeColorArrow arr = arr # fc yellowgreen # lc yellowgreen

shockColorArrow :: Diagram B -> Diagram B
shockColorArrow arr = arr # fc white # lc cyan

freezeBarDiagram :: DDRConfig -> FreezeLength -> Diagram B
freezeBarDiagram conf len =
  freezeArrowBase (fromRational $ sparseY conf * len)
    # fc yellowgreen
    # lc yellowgreen

shockBarDiagram :: DDRConfig -> YPos -> Diagram B
shockBarDiagram conf yPos =
  shockArrowBase (fromRational $ sparseX conf * 3) # colorArrow yPos

changeBPMDiagram :: DDRConfig -> Width -> BPM -> Diagram B
changeBPMDiagram conf wid bpm =
  alignedText 0 0 bpm # fc red # fontSize (Diagrams.Prelude.local 0.6)
  <> hruleLeft (fromRational $ sparseX conf * (wid + 2.5)) # lwL 0.1 # lc red

stopDiagram :: DDRConfig -> Width -> BPM -> Diagram B
stopDiagram conf wid bpm =
  alignedText 0 1 bpm # fc gray # fontSize (Diagrams.Prelude.local 0.6)
  <> hruleLeft (fromRational $ sparseX conf * (wid + 2.5)) # lwL 0.1 # lc gray


hruleLeft :: (InSpace V2 n t, TrailLike t) => n -> t
hruleLeft d = trailLike $ trailFromSegments [straight $ r2 (d, 0)] `at` p2 (0,0) 

-- # Base Diagrams

freezeArrowBase :: Double -> Diagram B
freezeArrowBase h =
  rect (1 - 1 / 2 / arrowWeight) (h - 1 / 2 / arrowWeight)
    # lwL (1 / 2 / arrowWeight)
    # frame (1 / 4 / arrowWeight)
  where arrowWeight = (4 * sqrt 2) + 1

shockArrowBase :: Double -> Diagram B
shockArrowBase w = rect w 0.4 # lwG (1 / 2 / arrowWeight) # frame
  (1 / 4 / arrowWeight)
  where arrowWeight = (4 * sqrt 2) + 1

arrowBase :: Diagram B
arrowBase =
  polygon
      (with & polyType .~ PolySides
        [ 90 @@ deg
        , 90 @@ deg
        , 225 @@ deg
        , 90 @@ deg
        , 90 @@ deg
        , 225 @@ deg
        , 90 @@ deg
        , 90 @@ deg
        ]
        [ 3 * rt2
        , rt2
        , 2 * rt2 - 1
        , 4 - (rt2 / 2)
        , rt2
        , 4 - (rt2 / 2)
        , 2 * rt2 - 1
        , rt2
        ]
      )
    # rotate (45 @@ deg)
    # translateX (-rt2 / 3)
    # lwL (1 / 2)
    # frame (1 / 4)
    # scale (1 / ((4 * rt2) + 1)) -- 3 * rt2 (longest edge) + 1 + rt2 (line weight)
  where rt2 = sqrt 2
