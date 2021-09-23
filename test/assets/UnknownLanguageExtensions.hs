{-# LANGUAGE UnkownLanguageExtension #-}

module UnkownLanguageExtension where

import Prelude

-- TypeApplications
-- > id @(Maybe [Char]) test ==> (Just "a")
test :: Maybe [Char]
test = Just "a"
