module Haskell.Verified.Examples.Verified (Verified (..)) where

import NriPrelude

data Verified
  = Verified
  | Unverified Text Text
  deriving (Show)