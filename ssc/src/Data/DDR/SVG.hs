{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- To live view: use vscode SVG Viewer

module Data.DDR.SVG where

import           Data.DDR
import qualified RIO.Vector                    as V
import           Diagrams.Prelude        hiding ( union )
import           Diagrams.Backend.SVG.CmdLine
-- import Diagrams.TwoD.Path.Boolean

ddrDiagram :: DDR -> Diagram B
ddrDiagram = vcat . map noteDiagram . V.toList

-- TODO: bpm, stop is not implemented
noteDiagram :: Note -> Diagram B
noteDiagram (Note (AbsBeat m d n) e) =
  extrudeBottom emptyNote . (|||) measureText $ case e of
    Nothing -> strutY 1
    Just (Event arr _bpm _stop) ->
      maybe (strutY 1) (applyColor n d . arrowDiagram) arr
 where
  emptyNote = -1 + (8 / fromIntegral d) :: Double
  measureText =
    (topLeftText (if n == 0 then show m else "") # fontSizeL 0.5)
      ||| strutX 1
      ||| (topLeftText (show n) # fontSizeL 0.5)
      ||| strutX 1

arrowDegs :: [Angle Double]
arrowDegs = [180 @@ deg, 270 @@ deg, 90 @@ deg, 0 @@ deg]

applyColor :: Int -> Int -> Diagram B -> Diagram B
applyColor numer denom arr
  | numer * 4 `mod` denom == 0  = arr # fc pink # lc darkred
  | numer * 8 `mod` denom == 0  = arr # fc lightblue # lc indigo
  | numer * 16 `mod` denom == 0 = arr # fc yellow # lc darkgoldenrod
  | otherwise                   = arr # fc yellowgreen # lc green

arrowDiagram :: Arrow -> Diagram B
arrowDiagram (Single l d u r) = foldr1 (|||) $ zipWith
  (\degree -> maybe (strutX 1) (rotate degree . fromNoteType))
  arrowDegs
  [l, d, u, r]

-- TODO: freeze, release, shock is not implemented
fromNoteType :: NoteType -> Diagram B
fromNoteType Normal  = arrowBase
fromNoteType Freeze  = arrowBase
fromNoteType Release = arrowBase
fromNoteType Shock   = arrowBase

a :: Arrow
a = Single Nothing (Just Normal) Nothing Nothing

b :: Arrow
b = Single (Just Normal) (Just Normal) Nothing (Just Normal)

c :: Arrow
c = Single (Just Normal) (Just Normal) (Just Normal) (Just Normal)

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
    -- # showEnvelope
  where rt2 = sqrt 2

-- union is does not work ...
-- arrowBase_ :: Diagram B
-- arrowBase_ = 
--   [ roundedRect 1 3 0.5 # translateX 1
--   , roundedRect 3 1 0.5 # translateY 1
--   , roundedRect 3 1 0.5 # translateY 1
--   , roundedRect (2 * sqrt 2 + 1) 1 0.5 # rotate (45 @@ deg)
--   ]
--   # mconcat
--   # union Winding
--   # strokePath
--   # frame 0.1
