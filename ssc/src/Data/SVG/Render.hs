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
objectDiagram conf ShockBar {..} = (mkPos conf xPos yPos, shockBarDiagram conf)

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

shockBarDiagram :: DDRConfig -> Diagram B
shockBarDiagram conf =
  shockArrowBase (fromRational $ sparseX conf * 4) # fc white # lc cyan

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
