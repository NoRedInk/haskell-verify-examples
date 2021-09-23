Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "Simple"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/passing/Simple.hs" 1 1 17 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/passing/Simple.hs" 1 1 1 1
                  , SrcSpan "test/assets/passing/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/passing/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/passing/Simple.hs" 5 1 5 1
                  , SrcSpan "test/assets/passing/Simple.hs" 16 1 16 1
                  , SrcSpan "test/assets/passing/Simple.hs" 17 1 17 1
                  , SrcSpan "test/assets/passing/Simple.hs" 17 1 17 1
                  ]
              }
        , languageExtensions = [ "NoImplicitPrelude" ]
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
      [ CodeBlockComment
          (VerifiedExample
             (SrcSpan "test/assets/passing/Simple.hs" 9 1 10 11)
             [ "test" , "==> 1" ])
      , CodeBlockComment
          (VerifiedExample
             (SrcSpan "test/assets/passing/Simple.hs" 12 1 13 11)
             [ "test + test" , "==> 2" ])
      , CodeBlockComment
          (UnverifiedExample
             (SrcSpan "test/assets/passing/Simple.hs" 15 1 15 17)
             [ "test + test" ])
      ]
  }