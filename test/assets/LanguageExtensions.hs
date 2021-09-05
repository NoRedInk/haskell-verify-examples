{-# LANGUAGE TypeApplications #-}

module LanguageExtensions where

-- TypeApplications
-- > Prelude.id @(Maybe [Char]) test ==> (Just "a")
test :: Maybe [Char]
test = Just "a"