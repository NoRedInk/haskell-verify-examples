{-# LANGUAGE CPP #-}

module Cpp where

#if __GLASGOW_HASKELL__ > 710
uniqueValue = 1
#else
uniqueValue = 2
#endif

-- > uniqueValue ==> 2