Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "Simple"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 1 1 17 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/Simple.hs" 1 1 1 1
                  , SrcSpan "test/assets/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/Simple.hs" 5 1 5 1
                  , SrcSpan "test/assets/Simple.hs" 16 1 16 1
                  , SrcSpan "test/assets/Simple.hs" 17 1 17 1
                  , SrcSpan "test/assets/Simple.hs" 17 1 17 1
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
        }
  , comments =
      [ PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 7 1 8 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 9 1 10 11
                 , srcInfoPoints = []
                 }
             , "test\n==> 1"
             ))
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 11 1 11 3
              , srcInfoPoints = []
              }
          , ""
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 12 1 13 11
                 , srcInfoPoints = []
                 }
             , "test + test\n==> 2"
             ))
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 14 1 14 3
              , srcInfoPoints = []
              }
          , ""
          )
      , CodeBlockComment
          (UnverifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 15 1 15 17
                 , srcInfoPoints = []
                 }
             , "test + test"
             ))
      ]
  }