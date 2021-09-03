ModuleWithExamples
  { moduleName =
      ( SrcSpanInfo
          { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 1 1 11 1
          , srcInfoPoints =
              [ SrcSpan "test/assets/Simple.hs" 1 1 1 1
              , SrcSpan "test/assets/Simple.hs" 1 1 1 1
              , SrcSpan "test/assets/Simple.hs" 10 1 10 1
              , SrcSpan "test/assets/Simple.hs" 11 1 11 1
              , SrcSpan "test/assets/Simple.hs" 11 1 11 1
              ]
          }
      , "Foo"
      )
  , languageExtensions = []
  , comments =
      [ Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 3 1 4 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 5 1 6 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 7 1 7 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 8 1 9 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  , examples =
      [ VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 5 1 6 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/Simple.hs" 8 1 9 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  }