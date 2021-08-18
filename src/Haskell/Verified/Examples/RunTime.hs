module Haskell.Verified.Examples.RunTime ((==>)) where

infixl 0 ==>

(==>) :: Eq a => a -> a -> Bool
x ==> y = x == y
