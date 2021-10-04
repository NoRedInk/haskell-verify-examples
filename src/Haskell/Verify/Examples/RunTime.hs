module Haskell.Verify.Examples.RunTime ((==>), (==?)) where

import Haskell.Verify.Examples.Verified (Verified (..))
import qualified Prelude

infixl 0 ==>

(==>) :: (Prelude.Show a, Prelude.Eq a) => a -> a -> Verified
x ==> y =
  if x Prelude.== y
    then Verified
    else Unverified (Prelude.show x) (Prelude.show y)

infixl 0 ==?

(==?) :: (Prelude.Show a) => a -> b -> Verified
x ==? _ =
  HelpTodo (Prelude.show x)
