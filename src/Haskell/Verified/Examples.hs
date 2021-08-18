module Haskell.Verified.Examples
  ( moduleWithExamples,
    Example (..),
    exampleFromText,
    Comment (..),
    run,
  )
where

import qualified Data.Foldable as Foldable
import qualified Language.Haskell.Exts.Comments as LHE.Comments
import qualified Language.Haskell.Exts.Lexer as LHE.Lexer
import qualified Language.Haskell.Exts.Parser as LHE
import Language.Haskell.Exts.SrcLoc ((<++>))
import qualified Language.Haskell.Exts.SrcLoc as LHE.SrcLoc
import qualified Language.Haskell.Exts.Syntax as LHE.Syntax
import qualified Language.Haskell.Interpreter as Hint
import NriPrelude
import qualified Prelude

data ModuleWithExamples = ModuleWithExamples
  { moduleName :: Text,
    comments :: List Comment,
    examples :: List Example
  }
  deriving (Show, Eq)

data Comment
  = Comment (LHE.SrcLoc.SrcSpanInfo, Text)
  | ExampleComment (LHE.SrcLoc.SrcSpanInfo, Text)
  deriving (Show, Eq)

data Example
  = VerifiedExample (LHE.SrcLoc.SrcSpanInfo, Text)
  | UnverifiedExample (LHE.SrcLoc.SrcSpanInfo, Text)
  deriving (Show, Eq)

-- TODO imports need to support qualified and stuff. This is just a hack to see how things work so far.
-- We can use setImportsQ.
-- And obviously need to parse it.
run :: [Text] -> Example -> Prelude.IO (Result Text Bool)
run imports example =
  case example of
    VerifiedExample (_, code) -> do
      result <- eval imports code
      case result of
        Prelude.Left err ->
          let _ = Debug.log "interpret error" err
           in Prelude.pure (Err (Debug.toString err))
        Prelude.Right execResult -> Prelude.pure (Ok execResult)
    UnverifiedExample (_, _code) -> Debug.todo "TODO"

eval :: [Text] -> Text -> Prelude.IO (Prelude.Either Hint.InterpreterError Bool)
eval imports s =
  Hint.runInterpreter <| do
    Hint.loadModules ["src/Haskell/Verified/Examples/RunTime.hs"]
    Hint.setImports ("NriPrelude" : "Haskell.Verified.Examples.RunTime" : List.map Text.toList imports)
    Hint.interpret (Text.toList s) (Hint.as :: Bool)

exampleFromText :: Text -> Maybe Example
exampleFromText val =
  ExampleComment (LHE.SrcLoc.noSrcSpan, val)
    |> toExamples

moduleWithExamples :: Text -> ModuleWithExamples
moduleWithExamples source =
  case LHE.parseModuleWithComments LHE.defaultParseMode (Text.toList source) of
    LHE.ParseOk (LHE.Syntax.Module _ (Just (LHE.Syntax.ModuleHead _ (LHE.Syntax.ModuleName _ name) _ _)) _ _ _, cs) ->
      let comments = toComments cs
          examples = List.filterMap toExamples comments
       in ModuleWithExamples
            { moduleName = Text.fromList name,
              comments,
              examples
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
                then ExampleComment (LHE.SrcLoc.noInfoSpan srcSpan, Text.dropLeft 3 val_)
                else Comment (LHE.SrcLoc.noInfoSpan srcSpan, val_)
      )
    |> mergeComments

toExamples :: Comment -> Maybe Example
toExamples (Comment _) = Nothing
toExamples (ExampleComment (srcLocInfo, source)) =
  case LHE.Lexer.lexTokenStream (Text.toList source) of
    LHE.ParseOk tokens ->
      let verified = Foldable.any ((== LHE.Lexer.VarSym "==>") << LHE.Lexer.unLoc) tokens
       in Just
            <| if verified
              then VerifiedExample (srcLocInfo, source)
              else UnverifiedExample (srcLocInfo, source)
    LHE.ParseFailed _ msg ->
      let _ = Debug.log "msg" msg
       in Debug.todo "TODO"

mergeComments :: List Comment -> List Comment
mergeComments cs =
  List.foldl
    ( \c xs ->
        case (c, xs) of
          (_, []) -> [c]
          (Comment (x, y), Comment (px, py) : rest) ->
            Comment (x <++> px, py ++ "\n" ++ y) : rest
          (ExampleComment (x, y), ExampleComment (px, py) : rest) ->
            ExampleComment (x <++> px, py ++ "\n" ++ y) : rest
          (c', prev : rest) ->
            c' : prev : rest
    )
    []
    cs
    |> List.reverse
