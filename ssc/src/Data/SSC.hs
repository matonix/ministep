module Data.SSC
  ( decode
  )
where

import           RIO                     hiding ( first )
import           Text.Megaparsec
import           Data.Bifunctor

import           Data.SSC.Parser
import           Data.SSC.Types

decode :: Text -> Either String SSC
decode = first errorBundlePretty . decodeHelper
