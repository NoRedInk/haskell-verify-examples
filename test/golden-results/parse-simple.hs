Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "Foo"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 1 1 15 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/Simple.hs" 1 1 1 1
                  , SrcSpan "test/assets/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/Simple.hs" 3 1 3 1
                  , SrcSpan "test/assets/Simple.hs" 5 1 5 1
                  , SrcSpan "test/assets/Simple.hs" 14 1 14 1
                  , SrcSpan "test/assets/Simple.hs" 15 1 15 1
                  , SrcSpan "test/assets/Simple.hs" 15 1 15 1
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
      [ Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 7 1 8 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 9 1 10 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 11 1 11 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 12 1 13 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  , examples =
      [ VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 9 1 10 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 12 1 13 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  }