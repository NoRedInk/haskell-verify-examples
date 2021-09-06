{-# LANGUAGE NoImplicitPrelude #-}

module InternalFunction () where

import NriPrelude 

-- Test that we have access in the interpreter to functions that are not exported 
-- (what hint calls a top level module)
--
-- > unexposed 1 ==> 2
unexposed :: Int -> Int
unexposed = (+ 1)