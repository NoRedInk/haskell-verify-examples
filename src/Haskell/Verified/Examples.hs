module Haskell.Verified.Examples
  ( comments,
    Comment (..),
  )
where

import NriPrelude

data Comment = Comment {unComment :: Text}
  deriving (Show, Eq)

comments :: Text -> List Comment
comments _source = [Comment "-- hello world"]
