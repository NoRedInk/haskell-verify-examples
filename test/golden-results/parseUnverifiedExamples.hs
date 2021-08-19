ModuleWithExamples
  { moduleName =
      ( SrcSpanInfo
          { srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 13 1
          , srcInfoPoints =
              [ SrcSpan "<unknown>.hs" 1 1 1 1
              , SrcSpan "<unknown>.hs" 1 1 1 1
              , SrcSpan "<unknown>.hs" 12 1 12 1
              , SrcSpan "<unknown>.hs" 13 1 13 1
              , SrcSpan "<unknown>.hs" 13 1 13 1
              ]
          }
      , "Foo"
      )
  , comments =
      [ Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 3 1 4 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 5 1 6 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 7 1 7 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 8 1 8 10
              , srcInfoPoints = []
              }
          , "test"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 9 1 9 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 10 1 11 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  , examples =
      [ VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 5 1 6 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , UnverifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 8 1 8 10
              , srcInfoPoints = []
              }
          , "test"
          )
      , VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "<unknown>.hs" 10 1 11 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 2"
          )
      ]
  }