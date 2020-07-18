{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- To live view: use vscode SVG Viewer

module Data.DDR.SVG where

import           RIO                     hiding ( Arrow )
import           Data.DDR
import qualified RIO.Map                       as Map
import qualified RIO.Vector                    as V
import           Diagrams.Prelude        hiding ( arrow )
import           Diagrams.Backend.SVG.CmdLine

type Pos = Point V2 Double
type AbsDiagrams = [(Pos, Diagram B)]
data NoteAlign = Measure | Numerator | NoteSingle Double
type FreezeMap = [Map AbsBeat AbsBeat]
type Arrows = [Arrow]

data DDRConfig = DDRConfig
  { sparseX :: Double
  , sparseY :: Double
  } deriving Show

defaultDDRConfig :: DDRConfig
defaultDDRConfig = DDRConfig 1 8

-- # Diagrams

ddrDiagram :: DDR Arrow -> Diagram B
ddrDiagram ddr = position
  $ concatMap (noteDiagram defaultDDRConfig freezeMap) ddr
  where freezeMap = map (toFreezeMap ddr) [left, down, up, right]

toFreezeMap :: DDR Arrow -> (Panel Arrow -> Arrow) -> Map AbsBeat AbsBeat
toFreezeMap ddr = Map.fromList . mkFreezeReleasePair . eachDirection ddr

mkFreezeReleasePair :: [(AbsBeat, NoteType)] -> [(AbsBeat, AbsBeat)]
mkFreezeReleasePair = go [] Nothing
 where
  go acc _        []                  = acc
  go acc Nothing  ((a, Freeze ) : xs) = go acc (Just a) xs
  go acc (Just a) ((b, Release) : xs) = go ((a, b) : acc) Nothing xs
  go acc x        (_            : xs) = go acc x xs

eachDirection :: DDR Arrow -> (Panel Arrow -> Arrow) -> [(AbsBeat, NoteType)]
eachDirection ddr dir' =
  [ (ab, nt) | (ab, Just nt) <- map (absBeat &&& pick) $ V.toList ddr ]
  where pick x = event x >>= arrow >>= dir'

-- # Note Level Diagrams

noteDiagram :: DDRConfig -> FreezeMap -> Note Arrow -> AbsDiagrams
noteDiagram conf frmap (Note ab e) = -- TODO
  measureText conf ab <> eventDiagram conf ab frmap e

measureText :: DDRConfig -> AbsBeat -> AbsDiagrams
measureText conf ab@(AbsBeat m _d n) =
  [ (mkPos conf ab Measure  , text (showOrEmpty n m) # fontSizeL 0.5)
  , (mkPos conf ab Numerator, text (show n) # fontSizeL 0.5)
  ]
 where
  showOrEmpty 0 m' = show m'
  showOrEmpty _ _  = mempty

eventDiagram
  :: DDRConfig -> AbsBeat -> FreezeMap -> Maybe (Event Arrow) -> AbsDiagrams
eventDiagram _ _ _ Nothing = mempty
eventDiagram conf ab frmap (Just (Event arr _bpm _stop)) =
  maybe [] (arrowDiagram conf ab frmap) arr

-- # Arrow Level Diagrams

arrowDiagram :: DDRConfig -> AbsBeat -> FreezeMap -> Panel Arrow -> AbsDiagrams
arrowDiagram conf ab frmap arr =
  normalArrows conf ab as
    <> shockArrow conf ab as
    <> freezeArrows conf ab frmap as
  where as = panelToList arr

normalArrows :: DDRConfig -> AbsBeat -> Arrows -> AbsDiagrams
normalArrows conf ab as = catMaybes
  $ zipWith3 normalArrow [0 ..] (cycle arrowDegs) as
 where
  normalArrow x ad nt = do
    nt' <- nt
    let normalPosition = mkPos conf ab (NoteSingle x)
        normalDiagram  = rotate ad $ fromNoteType ab nt'
    pure (normalPosition, normalDiagram)

arrowDegs :: [Angle Double]
arrowDegs = [180 @@ deg, 270 @@ deg, 90 @@ deg, 0 @@ deg]

shockArrow :: DDRConfig -> AbsBeat -> Arrows -> AbsDiagrams
shockArrow conf ab as = if any isJustShock as
  then pure (shockPosition, shockDiagram)
  else mempty
 where
  shockPosition = mkPos conf ab (NoteSingle 1.5)
  shockDiagram  = shockArrowBase 4 # fc white # lc cyan
  isJustShock (Just Shock) = True
  isJustShock _            = False

freezeArrows :: DDRConfig -> AbsBeat -> FreezeMap -> Arrows -> AbsDiagrams
freezeArrows (DDRConfig sx sy) ab frmap as = catMaybes
  $ zipWith3 freezeArrow [0 ..] frmap as
 where
  freezeArrow x eachMap nt = do
    nt' <- nt
    case nt' of
      Freeze -> do
        release <- Map.lookup ab eachMap
        let h              = yPos sy ab - yPos sy release
            freezePosition = p2 (xPos sx (NoteSingle x), yPos sy ab - (h / 2))
            freezeDiagram  = freezeArrowBase h # fc yellowgreen # lc yellowgreen
        pure (freezePosition, freezeDiagram)
      _ -> mzero

fromNoteType :: AbsBeat -> NoteType -> Diagram B
fromNoteType ab Normal  = arrowBase # applyColor ab
fromNoteType ab Freeze  = arrowBase # fc yellowgreen # applyColor ab
fromNoteType _  Release = arrowBase # fc yellowgreen # lc yellowgreen
fromNoteType ab Shock   = arrowBase # fc white # applyColor ab

applyColor :: AbsBeat -> Diagram B -> Diagram B
applyColor (AbsBeat _m denom numer) arr
  | numer * 4 `mod` denom == 0  = arr # fc pink # lc darkred
  | numer * 8 `mod` denom == 0  = arr # fc lightblue # lc indigo
  | numer * 16 `mod` denom == 0 = arr # fc yellow # lc darkgoldenrod
  | otherwise                   = arr # fc yellowgreen # lc green

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

-- # Helper Functions

mkPos :: DDRConfig -> AbsBeat -> NoteAlign -> Pos
mkPos (DDRConfig sx sy) ab noteAlign = p2 (xPos sx noteAlign, yPos sy ab)

-- | return y-axis positon and each measure has 1 height.
yPos :: Double -> AbsBeat -> Double
yPos sparse (AbsBeat m d n) =
  -1 * sparse * (fromIntegral m + fromIntegral n / fromIntegral d)

xPos :: Double -> NoteAlign -> Double
xPos sparse Measure        = sparse * 0.5
xPos sparse Numerator      = sparse * 1.5
xPos sparse (NoteSingle i) = sparse * (2.5 + i)
