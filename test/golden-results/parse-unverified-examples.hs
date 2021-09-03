ModuleWithExamples
  { moduleName = Just "Foo"
  , moduleSource =
      SrcSpanInfo
        { srcInfoSpan =
            SrcSpan "test/assets/UnverifiedExamples.hs" 1 1 22 1
        , srcInfoPoints =
            [ SrcSpan "test/assets/UnverifiedExamples.hs" 1 1 1 1
            , SrcSpan "test/assets/UnverifiedExamples.hs" 1 1 1 1
            , SrcSpan "test/assets/UnverifiedExamples.hs" 12 1 12 1
            , SrcSpan "test/assets/UnverifiedExamples.hs" 22 1 22 1
            , SrcSpan "test/assets/UnverifiedExamples.hs" 22 1 22 1
            ]
        }
  , languageExtensions = []
  , comments =
      [ Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/UnverifiedExamples.hs" 3 1 4 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 5 1 6 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/UnverifiedExamples.hs" 7 1 7 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 8 1 8 10
              , srcInfoPoints = []
              }
          , "test"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/UnverifiedExamples.hs" 9 1 9 3
              , srcInfoPoints = []
              }
          , ""
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 10 1 11 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 3"
          )
      , Comment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 14 1 15 3
              , srcInfoPoints = []
              }
          , " | more stuff\n"
          )
      , ExampleComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 16 1 21 21
              , srcInfoPoints = []
              }
          , "[ 1\n, 2\n, 3\n, 4\n] |> map (+ 1)\n==> [ 2, 4, 5 ]"
          )
      ]
  , examples =
      [ VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 5 1 6 11
              , srcInfoPoints = []
              }
          , "test\n==> 1"
          )
      , UnverifiedExample
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 8 1 8 10
              , srcInfoPoints = []
              }
          , "test"
          )
      , VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 10 1 11 11
              , srcInfoPoints = []
              }
          , "test + test\n==> 3"
          )
      , VerifiedExample
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 16 1 21 21
              , srcInfoPoints = []
              }
          , "[ 1\n, 2\n, 3\n, 4\n] |> map (+ 1)\n==> [ 2, 4, 5 ]"
          )
      ]
  }