Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "Simple"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 1 1 20 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/Simple.hs" 1 1 1 1
                  , SrcSpan "test/assets/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/Simple.hs" 5 1 5 1
                  , SrcSpan "test/assets/Simple.hs" 16 1 16 1
                  , SrcSpan "test/assets/Simple.hs" 19 1 19 1
                  , SrcSpan "test/assets/Simple.hs" 20 1 20 1
                  , SrcSpan "test/assets/Simple.hs" 20 1 20 1
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
            [ LanguageExtension { unLanguageExtension = "NoImplicitPrelude" } ]
        }
  , comments =
      [ [ CodeBlockComment
            (VerifiedExample
               (SrcSpan "test/assets/Simple.hs" 9 1 10 11) [ "test" , "==> 1" ])
        , CodeBlockComment
            (VerifiedExample
               (SrcSpan "test/assets/Simple.hs" 12 1 13 11)
               [ "test + test" , "==> 2" ])
        , CodeBlockComment
            (UnverifiedExample
               (SrcSpan "test/assets/Simple.hs" 15 1 15 17) [ "test + test" ])
        ]
      , [ CodeBlockComment
            (VerifiedExample
               (SrcSpan "test/assets/Simple.hs" 18 1 18 25)
               [ "test2 + test2 ==> 4" ])
        ]
      ]
  }