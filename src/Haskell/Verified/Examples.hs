module Haskell.Verified.Examples
  ( moduleWithExamples,
    Comment (..),
  )
where

import qualified Language.Haskell.Exts.Comments as LHE.Comments
import qualified Language.Haskell.Exts.Parser as LHE
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified Language.Haskell.Exts.Syntax as LHE.Syntax
import NriPrelude

data ModuleWithExamples = ModuleWithExamples
  { moduleName :: Text,
    comments :: List Comment
  }
  deriving (Show, Eq)

data Comment
  = Comment (List (LHE.SrcLoc.SrcSpan, Text))
  | ExampleComment (List (LHE.SrcLoc.SrcSpan, Text))
  deriving (Show, Eq)

moduleWithExamples :: Text -> ModuleWithExamples
moduleWithExamples source =
  case LHE.parseModuleWithComments LHE.defaultParseMode (Text.toList source) of
    LHE.ParseOk (LHE.Syntax.Module _ (Just (LHE.Syntax.ModuleHead _ (LHE.Syntax.ModuleName _ name) _ _)) _ _ _, cs) ->
      ModuleWithExamples
        { moduleName = Text.fromList name,
          comments = toComments cs
        }
    LHE.ParseOk (LHE.Syntax.Module _ Nothing _ _ _, _) ->
      Debug.todo "TODO no module head"
    LHE.ParseOk _ ->
      Debug.todo "TODO unsupported module type"
    LHE.ParseFailed _ _msg ->
      Debug.todo "TODO"

toComments :: List LHE.Comments.Comment -> List Comment
toComments cs =
  cs
    |> List.map
      ( \(LHE.Comments.Comment _ srcSpan val) ->
          let val_ = Text.fromList val
           in if Text.startsWith " > " val_
                then ExampleComment [(srcSpan, Text.dropLeft 3 val_)]
                else Comment [(srcSpan, val_)]
      )
    |> mergeComments

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
