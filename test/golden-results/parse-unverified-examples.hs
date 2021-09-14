Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "UnverifiedExamples"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 1 1 35 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/UnverifiedExamples.hs" 1 1 1 1
                  , SrcSpan "test/assets/UnverifiedExamples.hs" 3 1 3 1
                  , SrcSpan "test/assets/UnverifiedExamples.hs" 3 1 3 1
                  , SrcSpan "test/assets/UnverifiedExamples.hs" 5 1 5 1
                  , SrcSpan "test/assets/UnverifiedExamples.hs" 16 1 16 1
                  , SrcSpan "test/assets/UnverifiedExamples.hs" 35 1 35 1
                  , SrcSpan "test/assets/UnverifiedExamples.hs" 35 1 35 1
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
      [ PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/UnverifiedExamples.hs" 7 1 8 3
              , srcInfoPoints = []
              }
          , " hello world\n"
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan =
                     SrcSpan "test/assets/UnverifiedExamples.hs" 9 1 10 11
                 , srcInfoPoints = []
                 }
             , "test\n==> 1"
             ))
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 11 1 11 3
              , srcInfoPoints = []
              }
          , ""
          )
      , CodeBlockComment
          (UnverifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan =
                     SrcSpan "test/assets/UnverifiedExamples.hs" 12 1 12 10
                 , srcInfoPoints = []
                 }
             , "test"
             ))
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 13 1 13 3
              , srcInfoPoints = []
              }
          , ""
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan =
                     SrcSpan "test/assets/UnverifiedExamples.hs" 14 1 15 11
                 , srcInfoPoints = []
                 }
             , "test + test\n==> 3"
             ))
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 18 1 19 3
              , srcInfoPoints = []
              }
          , " | more stuff\n"
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan =
                     SrcSpan "test/assets/UnverifiedExamples.hs" 20 1 25 21
                 , srcInfoPoints = []
                 }
             , "[ 1\n, 2\n, 3\n, 4\n] |> map (+ 1)\n==> [ 2, 4, 5 ]"
             ))
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan =
                  SrcSpan "test/assets/UnverifiedExamples.hs" 27 1 28 3
              , srcInfoPoints = []
              }
          , " | compilation fails\n"
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan =
                     SrcSpan "test/assets/UnverifiedExamples.hs" 29 1 34 14
                 , srcInfoPoints = []
                 }
             , "[ 1\n, 2\n, 3\n, 4\n] |> map (+ 1)\n==> True"
             ))
      ]
  }