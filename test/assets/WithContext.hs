{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WithContext where

import NriPrelude

-- |
--
-- Setup for examples below:
--
-- @
-- import Prelude (String)
--
-- result :: Maybe String
-- result = Just "a"
-- @
--
-- Examples:
--
-- > identity @(Maybe [Char]) test ==> result
--
-- > Just "b" ==> result
test :: Maybe [Char]
test = Just "a"

-- |
--
-- Examples:
--
-- > testNoAccessToResult ==> result
testNoAccessToResult :: Maybe [Char]
testNoAccessToResult = Just "a"

-- |
--
-- Setup for examples below:
--
-- @
-- import qualified Prelude
--
-- result :: Maybe Prelude.String
-- result = Just "a"
-- @
--
-- Examples:
--
-- > test3 ==> result
test3 :: Maybe [Char]
test3 = Just "a"

-- |
--
-- Setup for examples below:
--
-- @
-- import WithContext
--
-- forContext = test4
-- @
--
-- Examples:
--
-- > forContext ==> Just "b"
test4 :: Maybe [Char]
test4 = Just "b"
