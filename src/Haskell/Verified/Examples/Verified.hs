module Haskell.Verified.Examples.Verified (Verified (..)) where

import qualified Prelude

data Verified
  = Verified
  | Unverified Prelude.String Prelude.String
  | NoExampleResult Prelude.String
  deriving (Prelude.Show)
