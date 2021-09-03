{-# LANGUAGE TypeApplications #-}

module LanguageExtensions where

test = Just "a"

-- TypeApplications
-- > id @(Maybe [Char]) test ==> (Just "a")
id :: a -> a
id x = x