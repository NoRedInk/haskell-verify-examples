module Haskell.Verified.Examples
  ( comments,
    Comment (..),
  )
where

import qualified Language.Haskell.Exts.Comments as HsParser.Comments
import qualified Language.Haskell.Exts.Parser as HsParser
import NriPrelude

data Comment = Comment {unComment :: Text}
  deriving (Show, Eq)

comments :: Text -> List Comment
comments source =
  case HsParser.parseModuleWithComments HsParser.defaultParseMode (Text.toList source) of
    HsParser.ParseOk (_, cs) ->
      cs
        |> List.map (\(HsParser.Comments.Comment _ _ val) -> Comment (Text.fromList val))
    HsParser.ParseFailed _ msg ->
      let _ = Debug.log "todo" msg
       in []
