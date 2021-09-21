Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "WithContext"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 1 1 22 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/WithContext.hs" 1 1 1 1
                  , SrcSpan "test/assets/WithContext.hs" 2 1 2 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 6 1 6 1
                  , SrcSpan "test/assets/WithContext.hs" 20 1 20 1
                  , SrcSpan "test/assets/WithContext.hs" 21 1 21 1
                  , SrcSpan "test/assets/WithContext.hs" 22 1 22 1
                  , SrcSpan "test/assets/WithContext.hs" 22 1 22 1
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
          (SrcSpan "test/assets/WithContext.hs" 10 1 15 5)
          [ "import NriPrelude"
          , ""
          , "result :: Maybe [Char]"
          , "result = Just \"a\""
          ]
      , CodeBlockComment
          (VerifiedExample
             (SrcSpan "test/assets/WithContext.hs" 17 1 17 46)
             [ "identity @(Maybe [Char]) test ==> result" ])
      , CodeBlockComment
          (VerifiedExample
             (SrcSpan "test/assets/WithContext.hs" 19 1 19 25)
             [ "Just \"b\" ==> result" ])
      ]
  }