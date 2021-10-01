Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "WithContext"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 1 1 51 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/WithContext.hs" 1 1 1 1
                  , SrcSpan "test/assets/WithContext.hs" 2 1 2 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 6 1 6 1
                  , SrcSpan "test/assets/WithContext.hs" 24 1 24 1
                  , SrcSpan "test/assets/WithContext.hs" 25 1 25 1
                  , SrcSpan "test/assets/WithContext.hs" 32 1 32 1
                  , SrcSpan "test/assets/WithContext.hs" 33 1 33 1
                  , SrcSpan "test/assets/WithContext.hs" 49 1 49 1
                  , SrcSpan "test/assets/WithContext.hs" 50 1 50 1
                  , SrcSpan "test/assets/WithContext.hs" 51 1 51 1
                  , SrcSpan "test/assets/WithContext.hs" 51 1 51 1
                  ]
              }
        , imports =
            [ ModuleImport
                { modName = "NriPrelude"
                , modQual = NotQualified
                , modImp = NoImportList
                }
            ]
        , moduleLanguageExtensions =
            [ LanguageExtension { unLanguageExtension = "TypeApplications" }
            , LanguageExtension { unLanguageExtension = "NoImplicitPrelude" }
            ]
        }
  , comments =
      [ Comment
          { codeBlocks =
              [ ContextBlock
                  (SrcSpan "test/assets/WithContext.hs" 12 1 17 5)
                  [ "import Prelude (String)"
                  , ""
                  , "result :: Maybe String"
                  , "result = Just \"a\""
                  ]
              , ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/WithContext.hs" 21 1 21 46)
                     [ "identity @(Maybe [Char]) test ==> result" ])
              , ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/WithContext.hs" 23 1 23 25)
                     [ "Just \"b\" ==> result" ])
              ]
          }
      , Comment
          { codeBlocks =
              [ ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/WithContext.hs" 31 1 31 37)
                     [ "testNoAccessToResult ==> result" ])
              ]
          }
      , Comment
          { codeBlocks =
              [ ContextBlock
                  (SrcSpan "test/assets/WithContext.hs" 39 1 44 5)
                  [ "import qualified Prelude"
                  , ""
                  , "result :: Maybe Prelude.String"
                  , "result = Just \"a\""
                  ]
              , ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/WithContext.hs" 48 1 48 22)
                     [ "test3 ==> result" ])
              ]
          }
      ]
  }