module Data.SVG.Types where

-- Render notes one by one
type Objects = [Object]

data Object =
  Note
    { xPos :: XPos
    , yPos :: YPos
    , direction :: Direction
    , noteType :: NoteType
    }
  | FreezeBar
    { xPos :: XPos
    , yPos :: YPos
    , direction :: Direction
    , freezeLength :: FreezeLength
    }
  | ShockBar
    { xPos :: XPos
    , yPos :: YPos
    }
  | ChangeBPM
    { yPos :: YPos
    , width :: Width
    , bpm :: BPM
    }
  | Stop
    { yPos :: YPos
    , width :: Width
    , stop :: Second
    } deriving Eq

-- Position is scaled by unit size (basically, we use eight beat as unit)
type XPos = Rational
type YPos = Rational
data Direction = L | D | U | R deriving Eq
data NoteType = Normal | Freeze | Release | Shock deriving Eq
type FreezeLength = Rational
type BPM = String
type Second = String
type Width = Rational

instance Ord NoteType where
  compare Normal  Normal  = EQ
  compare Normal  Freeze  = EQ
  compare Normal  Release = GT
  compare Normal  Shock   = EQ
  compare Freeze  Normal  = EQ
  compare Freeze  Freeze  = EQ
  compare Freeze  Release = GT
  compare Freeze  Shock   = EQ
  compare Release Normal  = LT
  compare Release Freeze  = LT
  compare Release Release = EQ
  compare Release Shock   = LT
  compare Shock   Normal  = EQ
  compare Shock   Freeze  = EQ
  compare Shock   Release = GT
  compare Shock   Shock   = EQ

instance Ord Object where
  compare a@Note{} b@Note{} = case compare (noteType a) (noteType b) of
    LT -> LT
    GT -> GT
    EQ -> compare (yPos a) (yPos b)
  compare Note{}        FreezeBar{}   = GT
  compare Note{}        ShockBar{}    = GT
  compare FreezeBar{}   Note{}        = LT
  compare a@FreezeBar{} b@FreezeBar{} = compare (yPos a) (yPos b)
  compare FreezeBar{}   ShockBar{}    = GT
  compare ShockBar{}    Note{}        = LT
  compare ShockBar{}    FreezeBar{}   = LT
  compare a@ShockBar{}  b@ShockBar{}  = compare (yPos a) (yPos b)
  compare ChangeBPM{} _ = LT
  compare _ ChangeBPM{} = GT
  compare Stop{} _ = LT
  compare _ Stop{} = GT