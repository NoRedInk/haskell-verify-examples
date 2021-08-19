module Haskell.Verified.Examples.RunTime ((==>)) where

import qualified Debug
import Haskell.Verified.Examples.Verified (Verified (..))
import NriPrelude
import Prelude hiding ((==))

infixl 0 ==>

(==>) :: (Show a, Eq a) => a -> a -> Verified
x ==> y =
  if x == y
    then Verified
    else Unverified (Debug.toString x) (Debug.toString y)
