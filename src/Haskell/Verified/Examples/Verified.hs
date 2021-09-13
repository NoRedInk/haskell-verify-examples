module Haskell.Verified.Examples.Verified (Verified (..)) where

import qualified Prelude

data Verified
  = Verified
  | Unverified Prelude.String Prelude.String
  | NoExampleResult
  deriving (Prelude.Show)
