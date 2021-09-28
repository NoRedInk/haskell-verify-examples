Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "WithContext"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 1 1 24 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/WithContext.hs" 1 1 1 1
                  , SrcSpan "test/assets/WithContext.hs" 2 1 2 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 6 1 6 1
                  , SrcSpan "test/assets/WithContext.hs" 22 1 22 1
                  , SrcSpan "test/assets/WithContext.hs" 23 1 23 1
                  , SrcSpan "test/assets/WithContext.hs" 24 1 24 1
                  , SrcSpan "test/assets/WithContext.hs" 24 1 24 1
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
      [ ContextBlockComment
          (SrcSpan "test/assets/WithContext.hs" 12 1 15 5)
          [ "result :: Maybe [Char]" , "result = Just \"a\"" ]
      , CodeBlockComment
          (VerifiedExample
             (SrcSpan "test/assets/WithContext.hs" 19 1 19 46)
             [ "identity @(Maybe [Char]) test ==> result" ])
      , CodeBlockComment
          (VerifiedExample
             (SrcSpan "test/assets/WithContext.hs" 21 1 21 25)
             [ "Just \"b\" ==> result" ])
      ]
  }