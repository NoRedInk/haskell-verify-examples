{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WithContext where

import NriPrelude

-- TypeApplications
--
-- @
-- result :: Maybe [Char]
-- result = Just "a"
-- @
--
-- > identity @(Maybe [Char]) test ==> result
test :: Maybe [Char]
test = Just "a"
