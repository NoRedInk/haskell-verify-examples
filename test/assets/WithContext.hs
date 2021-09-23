{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WithContext where

import NriPrelude

-- | TypeApplications
--
-- Setup for examples below:
--
-- @
-- result :: Maybe [Char]
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
