module Haskell.Verified.Examples.RunTime ((==>)) where

import Haskell.Verified.Examples.Verified (Verified (..))
import qualified Prelude

infixl 0 ==>

(==>) :: (Prelude.Show a, Prelude.Eq a) => a -> a -> Verified
x ==> y =
  if x Prelude.== y
    then Verified
    else Unverified (Prelude.show x) (Prelude.show y)
