module Imports where

import NriPrelude ((>>))
import Tuple (second)
import qualified Tuple
import Prelude hiding ((>>))

-- > (f >> f) 1 ==> f 1
f x = x

--
-- > second (True, False) ==> False
--
-- > Tuple.first (True, False) ==> True
