{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WithContext where

import NriPrelude

-- TypeApplications
--
-- @
-- import NriPrelude
--
-- result :: Maybe [Char]
-- result = Just "a"
-- @
--
-- > identity @(Maybe [Char]) test ==> result
--
-- > Just "b" ==> result
test :: Maybe [Char]
test = Just "a"
