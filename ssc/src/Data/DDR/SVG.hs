{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- To live view: use vscode SVG Viewer

module Data.DDR.SVG where

import           RIO
import           Data.DDR
import qualified RIO.Map                       as Map
import qualified RIO.Vector                    as V
import           Diagrams.Prelude        hiding ( union
                                                , arrow
                                                )
import           Diagrams.Backend.SVG.CmdLine
-- import Diagrams.TwoD.Path.Boolean

type Pos = Point V2 Double
type AbsDiagram = (Pos, Diagram B)
data NoteAlign = Measure | Numerator | NoteSingle Double
type FreezeMap = [Map AbsBeat AbsBeat]

data DDRConfig = DDRConfig
  { sparseX :: Double
  , sparseY :: Double
  } deriving Show

defaultDDRConfig :: DDRConfig
defaultDDRConfig = DDRConfig 1 8

ddrDiagram :: DDR -> Diagram B
ddrDiagram ddr =
  position . concatMap (noteDiagram defaultDDRConfig freezeMap) $ V.toList ddr
 where
  freezeMap = map toFreezeMap [left, down, up, right]
  toFreezeMap :: (Arrow -> Maybe NoteType) -> Map AbsBeat AbsBeat
  toFreezeMap dir = Map.fromList . reduce $ eachDirection dir
  reduce :: [(AbsBeat, NoteType)] -> [(AbsBeat, AbsBeat)]
  reduce = go [] Nothing
   where
    go acc _        []                  = acc
    go acc Nothing  ((a, Freeze ) : xs) = go acc (Just a) xs
    go acc (Just a) ((b, Release) : xs) = go ((a, b) : acc) Nothing xs
    go acc x        (_            : xs) = go acc x xs
  eachDirection :: (Arrow -> Maybe NoteType) -> [(AbsBeat, NoteType)]
  eachDirection dir' =
    [ (ab, nt) | (ab, Just nt) <- map (absBeat &&& pick) $ V.toList ddr ]
    where pick x = event x >>= arrow >>= dir'

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

-- TODO: bpm, stop is not implemented
noteDiagram :: DDRConfig -> FreezeMap -> Note -> [AbsDiagram]
noteDiagram conf frmap (Note ab e) = -- TODO
  measureText conf ab <> eventDiagram conf ab frmap e

measureText :: DDRConfig -> AbsBeat -> [AbsDiagram]
measureText conf ab@(AbsBeat m _d n) =
  [ ( mkPos conf ab Measure
    , text (if n == 0 then show m else "") # fontSizeL 0.5
    )
  , (mkPos conf ab Numerator, text (show n) # fontSizeL 0.5)
  ]

eventDiagram :: DDRConfig -> AbsBeat -> FreezeMap -> Maybe Event -> [AbsDiagram]
eventDiagram _ _ _ Nothing = mempty
eventDiagram conf ab frmap (Just (Event arr _bpm _stop)) =
  maybe [] (arrowDiagram conf ab frmap) arr

arrowDegs :: [Angle Double]
arrowDegs = [180 @@ deg, 270 @@ deg, 90 @@ deg, 0 @@ deg]

arrowDiagram :: DDRConfig -> AbsBeat -> FreezeMap -> Arrow -> [AbsDiagram]
arrowDiagram conf@(DDRConfig sx sy) ab frmap (Single l d u r) =
  arrows <> shockArrow <> freezeArrows
 where
  freezeArrows = catMaybes $ zipWith3 freezeArrow [0 ..] frmap [l, d, u, r]
   where
    freezeArrow x eachMap (Just Freeze) = Just
      ( p2 (xPos sx (NoteSingle x), yPos sy ab - (h / 2))
      , freezeArrowBase h # fc yellowgreen # lc yellowgreen
      )
     where
      Just release = Map.lookup ab eachMap
      h            = yPos sy ab - yPos sy release
    freezeArrow _ _ _ = Nothing
  shockArrow = case l of
    (Just Shock) ->
      [(mkPos conf ab (NoteSingle 1.5), shockArrowBase 4 # fc white # lc cyan)]
    _ -> mempty
  arrows = catMaybes $ zipWith3 f [0 ..] (cycle arrowDegs) [l, d, u, r]
   where
    f x ad nt = fmap (\nt' -> (pos x, dia ad nt')) nt
    pos x = mkPos conf ab (NoteSingle x)
    dia ad nt = rotate ad $ fromNoteType ab nt

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

freezeArrowBase :: Double -> Diagram B
freezeArrowBase h = rect (1 - 1 / 2 / arrowWeight) (h - 1 / 2 / arrowWeight) # lwL (1 / 2 / arrowWeight) # frame
  (1 / 4 / arrowWeight)
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
