ModuleWithExamples
  { moduleName =
      ( SrcSpanInfo
          { srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 9 9
          , srcInfoPoints =
              [ SrcSpan "<unknown>.hs" 1 1 1 1
              , SrcSpan "<unknown>.hs" 1 1 1 1
              , SrcSpan "<unknown>.hs" 9 1 9 1
              , SrcSpan "<unknown>.hs" 9 9 9 9
              ]
          }
      , "Foo"
      )
  , comments =
      [ Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 2 1 3 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 4 1 5 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 6 1 6 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 7 1 8 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  , examples =
      [ VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 4 1 5 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 7 1 8 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  }