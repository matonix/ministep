-- This Impl. is techfest7th version.
module Proto () where

import Data.Function
import Data.Array
import Data.Foldable
import Data.Ix
import Data.Maybe
import Control.Monad.Cont
import Control.Monad.Reader

data Panel = L | D | U | R
  deriving (Show, Eq)
data Foot = FL | FR
  deriving (Show, Eq, Ord, Ix)

type Cost = Int
type Step = (Panel, Foot)
type Panels = Array Int Panel
type Steps = Array Int Step
type DPArrayIx = (Int, Cost, Foot)
type DPArrayElem = (Cost, Maybe Foot)
type DPArray = Array DPArrayIx DPArrayElem

inf :: Int
inf = maxBound

k :: Cost
k = maxCost

sample :: Panels
sample = listArray (1, 4) [L, D, U, R]

type RCont r = ReaderT Env (Cont r)
type Env = (Int, Panels)

solve :: Panels -> Steps
solve panels = (`runCont` id) $ (`runReaderT` (length panels, panels)) $ do
  dp <- constructArray
  mini <- findMinimum dp
  extractTrace dp mini

constructArray :: RCont r DPArray
constructArray = do
  (n, panels) <- ask
  let 
    ix = ((1, 1, FL), (n, n * k, FR))
    dp = listArray ix $ map dec (range ix)
    dec :: DPArrayIx -> DPArrayElem
    dec (1, 1, FL) = (1, Nothing)
    dec (1, 1, FR) = (1, Nothing)
    dec (1, _, _) = (inf, Nothing)
    dec (i, j, f) = case cFLPrev `compare` cFRPrev of
      LT -> cfFL
      GT -> cfFR
      EQ -> if cFLPrev == inf then (inf, Nothing) else cfFL
      where
        (!!) = atOrError "constructArray"
        (!?) = atOrDefault (inf, Nothing) 
        prevElem (i, j, f) = if inRange (bounds dp) (i, j, f) then (j, Just f) else (inf, Nothing)
        cFL = cost (panels !! (i - 1), FL) (panels !! i, f)
        cFLPrev = fst $ dp !? (i - 1, j - cFL, FL)
        cfFL = prevElem (i - 1, j - cFL, FL)
        cFR = cost (panels !! (i - 1), FR) (panels !! i, f)
        cFRPrev = fst $ dp !? (i - 1, j - cFR, FR)
        cfFR = prevElem (i - 1, j - cFR, FR)
  return dp

findMinimum :: DPArray -> RCont r (Cost, Foot)
findMinimum dp = do
  (n, panels) <- ask
  let 
    (!!) = atOrError "findMinimum"
    min = head [ (j, f) 
               | f <- [FL, FR]
               , j <- [1..(n * k)]
               , fst (dp !! (n, j, f)) /= inf
               ] -- partial
  return min

extractTrace :: DPArray -> (Cost, Foot) -> RCont r Steps
extractTrace dp (jMin, fMin) = do
  (n, panels) <- ask
  let 
    (!!) = atOrError "extractTrace"
    steps = listArray (bounds panels) $ zip (toList panels) $ go (n, jMin, fMin) []
    go (1, _, f) fs = f : fs
    go (i, j, f) fs = go (i - 1, j', fromJust f') (f : fs) -- Partial
      where
        (j', f') = dp !! (i, j, f)
  return steps

-- given functions

cost :: Step -> Step -> Cost
cost (px, fx) (py, fy) = distance px py + alpha * foot fx fy
  where
    alpha = 2
    distance U D = 2
    distance D U = 2
    distance L R = 2
    distance R L = 2
    distance x y
      | x == y = 2
      | otherwise = 1 
    foot x y
      | x == y = 2
      | otherwise = 1 

maxCost :: Cost
maxCost = maximum [ cost (px, fx) (py, fy) 
                  | px <- [L, D, U, R]
                  , py <- [L, D, U, R]
                  , fx <- [FL, FR]
                  , fy <- [FL, FR]
                  ]
    
-- helper functions

atOrDefault :: Ix i => e -> Array i e -> i -> e
atOrDefault def arr i = if inRange (bounds arr) i then arr ! i else def

atOrError :: Ix i => String -> Array i e -> i -> e
atOrError name arr i = if inRange (bounds arr) i then arr ! i else error $ name ++ ": index out of bound"