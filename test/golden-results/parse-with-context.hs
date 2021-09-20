Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "WithContext"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 1 1 18 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/WithContext.hs" 1 1 1 1
                  , SrcSpan "test/assets/WithContext.hs" 2 1 2 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 6 1 6 1
                  , SrcSpan "test/assets/WithContext.hs" 16 1 16 1
                  , SrcSpan "test/assets/WithContext.hs" 17 1 17 1
                  , SrcSpan "test/assets/WithContext.hs" 18 1 18 1
                  , SrcSpan "test/assets/WithContext.hs" 18 1 18 1
                  ]
              }
        , languageExtensions = [ "TypeApplications" , "NoImplicitPrelude" ]
        , imports =
            [ ModuleImport
                { modName = "NriPrelude"
                , modQual = NotQualified
                , modImp = NoImportList
                }
            ]
        , importPaths = []
        , packageDbs = []
        }
  , comments =
      [ ContextBlockComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 10 1 13 5
              , srcInfoPoints = []
              }
          , [ "result :: Maybe [Char]" , "result = Just \"a\"" ]
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 15 1 15 46
                 , srcInfoPoints = []
                 }
             , [ "identity @(Maybe [Char]) test ==> result" ]
             ))
      ]
  }