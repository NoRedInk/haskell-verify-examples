{-# LANGUAGE NoImplicitPrelude #-}

module UnverifiedExamples where

import NriPrelude

-- hello world
--
-- > test
-- > ==> 1
--
-- > test
--
-- > test + test
-- > ==> 3
test = 1

-- | more stuff
--
-- > [ 1
-- > , 2
-- > , 3
-- > , 4
-- > ] |> map (+ 1)
-- > ==> [ 2, 4, 5 ]
