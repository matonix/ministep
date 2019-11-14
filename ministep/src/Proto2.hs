{-# LANGUAGE FlexibleContexts #-}
module Proto2 where

import           Data.Traversable
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Foldable
import           Data.Function

-- solve :: Notes -> Actions
-- solve = toActions . policyIteration θ γ . toMDP
--  where
--   θ         = undefined
--   γ         = undefined
--   toMDP     = undefined
--   toActions = undefined

type State = Int
type States = [State]
type Action = Char
type Actions = [Action]
type Transition = State -> Action -> State -> Double
type Reward = State -> Action -> Double
type Value = Map State Double
type Policy = Map State Action

policyIteration
  :: Double -> Double -> States -> Actions -> Transition -> Reward -> Policy
policyIteration γ θ states actions transition reward = policyEvalLoop
  initialValue
  initialPolicy
 where
  initialValue :: Value
  initialValue = mkValue states (const 0.0)
  initialPolicy :: Policy
  initialPolicy = mkPolicy states (const 'a')

  policyEvalLoop :: Value -> Policy -> Policy
  policyEvalLoop value policy =
    let p xs ys = (< θ) . sum $ Map.unionWith (\x y -> abs (x - y)) xs ys
        nextValue = whileDiff p $ iterate
          (policyEvaluation γ states actions transition reward)
          value
    in  policyImproveLoop nextValue policy

  policyImproveLoop :: Value -> Policy -> Policy
  policyImproveLoop value policy =
    let newPolicy = policyImprovement γ states actions transition reward value
    in  if newPolicy == policy
          then newPolicy
          else policyEvalLoop value newPolicy

  whileDiff :: (a -> a -> Bool) -> [a] -> a
  whileDiff rel xs = f xs $ tail xs
   where
    f (x : xs) (y : ys) | x `rel` y = x
                        | otherwise = f xs ys



policyEvaluation
  :: Double -> States -> Actions -> Transition -> Reward -> Value -> Value
policyEvaluation γ states actions transition reward value =
  mkValue states $ \s -> sumWith actions $ \a -> sumWith states
    $ \s' -> transition s a s' * (reward s a + γ * (value ! s'))

policyImprovement
  :: Double -> States -> Actions -> Transition -> Reward -> Value -> Policy
policyImprovement γ states actions transition reward value =
  mkPolicy states $ \s -> argmax actions $ \a -> sumWith states
    $ \s' -> transition s a s' * (reward s a + γ * (value ! s'))

argmax :: Ord b => [a] -> (a -> b) -> a
argmax xs f = fst . maximumBy (compare `on` snd) . zip xs $ map f xs

sumWith :: Num b => [a] -> (a -> b) -> b
sumWith xs f = sum $ map f xs

m !k = fromMaybe 0.0 $ Map.lookup k m

mkValue xs f = Map.fromList . zip xs $ map f xs

mkPolicy xs f = Map.fromList . zip xs $ map f xs

