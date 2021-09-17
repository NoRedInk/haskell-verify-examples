{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WithContext where

import NriPrelude

-- TypeApplications
--
-- @
-- import Prelude
--
-- result :: Maybe [Char]
-- result = Just "a"
-- @
--
-- > id @(Maybe [Char]) test ==> result
test :: Maybe [Char]
test = Just "a"
