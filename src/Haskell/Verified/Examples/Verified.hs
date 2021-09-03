module Haskell.Verified.Examples.Verified (Verified (..)) where

import Prelude

data Verified
  = Verified
  | Unverified String String
  | NoExampleResult String
  deriving (Show)
