module Haskell.Verified.Examples
  ( comments,
    Comment (..),
  )
where

import qualified Language.Haskell.Exts.Comments as HsParser.Comments
import qualified Language.Haskell.Exts.Parser as HsParser
import NriPrelude

data Comment
  = Comment Text
  | ExampleComment Text
  deriving (Show, Eq)

comments :: Text -> List Comment
comments source =
  case HsParser.parseModuleWithComments HsParser.defaultParseMode (Text.toList source) of
    HsParser.ParseOk (_, cs) ->
      cs
        |> List.map
          ( \(HsParser.Comments.Comment _ _ val) ->
              let val_ = Text.fromList val
               in if Text.startsWith " > " val_
                    then ExampleComment (Text.dropLeft 3 val_)
                    else Comment val_
          )
        |> List.foldl
          ( \c xs ->
              case (c, xs) of
                (_, []) -> [c]
                (Comment c', Comment prev : rest) ->
                  Comment (prev ++ "\n" ++ c') : rest
                (ExampleComment c', ExampleComment prev : rest) ->
                  ExampleComment (prev ++ "\n" ++ c') : rest
                (c', prev : rest) ->
                  c' : prev : rest
          )
          []
        |> List.reverse
    HsParser.ParseFailed _ msg ->
      let _ = Debug.log "todo" msg
       in []
