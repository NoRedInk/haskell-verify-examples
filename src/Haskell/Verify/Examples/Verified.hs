module Haskell.Verify.Examples.Verified (Verified (..)) where

import qualified Prelude

data Verified
  = Verified
  | Unverified Prelude.String Prelude.String
  | Todo
  | HelpTodo Prelude.String
  deriving (Prelude.Show)
