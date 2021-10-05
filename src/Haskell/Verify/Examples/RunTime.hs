module Haskell.Verify.Examples.RunTime ((==>), evaluteExampleTodo) where

import Haskell.Verify.Examples.Verified (Verified (..))
import qualified Prelude

infixl 0 ==>

(==>) :: (Prelude.Show a, Prelude.Eq a) => a -> a -> Verified
x ==> y =
  if x Prelude.== y
    then Verified
    else Unverified (Prelude.show x) (Prelude.show y)

evaluteExampleTodo :: (Prelude.Show a) => a -> Verified
evaluteExampleTodo x = HelpTodo (Prelude.show x)
