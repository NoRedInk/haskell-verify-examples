module Haskell.Verified.Examples
  ( comments,
    Comment (..),
  )
where

import qualified Language.Haskell.Exts.Comments as LHE.Comments
import qualified Language.Haskell.Exts.Parser as LHE
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import NriPrelude

data Comment
  = Comment (List (LHE.SrcLoc.SrcSpan, Text))
  | ExampleComment (List (LHE.SrcLoc.SrcSpan, Text))
  deriving (Show, Eq)

comments :: Text -> List Comment
comments source =
  case LHE.parseModuleWithComments LHE.defaultParseMode (Text.toList source) of
    LHE.ParseOk (_, cs) ->
      cs
        |> List.map
          ( \(LHE.Comments.Comment _ srcSpan val) ->
              let val_ = Text.fromList val
               in if Text.startsWith " > " val_
                    then ExampleComment [(srcSpan, Text.dropLeft 3 val_)]
                    else Comment [(srcSpan, val_)]
          )
        |> mergeComments
    LHE.ParseFailed _ msg ->
      let _ = Debug.log "todo" msg
       in []

mergeComments :: List Comment -> List Comment
mergeComments cs =
  List.foldl
    ( \c xs ->
        case (c, xs) of
          (_, []) -> [c]
          (Comment c', Comment prev : rest) ->
            Comment (prev ++ c') : rest
          (ExampleComment c', ExampleComment prev : rest) ->
            ExampleComment (prev ++ c') : rest
          (c', prev : rest) ->
            c' : prev : rest
    )
    []
    cs
    |> List.reverse
