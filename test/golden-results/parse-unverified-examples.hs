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
      [ Comment
          { codeBlocks =
              [ ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/UnverifiedExamples.hs" 9 1 10 11)
                     [ "test" , "==> 1" ])
              , ExampleBlock
                  (UnverifiedExample
                     (SrcSpan "test/assets/UnverifiedExamples.hs" 12 1 12 10)
                     [ "test" ])
              , ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/UnverifiedExamples.hs" 14 1 15 11)
                     [ "test + test" , "==> 3" ])
              ]
          }
      , Comment
          { codeBlocks =
              [ ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/UnverifiedExamples.hs" 20 1 25 21)
                     [ "[ 1"
                     , ", 2"
                     , ", 3"
                     , ", 4"
                     , "] |> map (+ 1)"
                     , "==> [ 2, 4, 5 ]"
                     ])
              ]
          }
      , Comment
          { codeBlocks =
              [ ExampleBlock
                  (VerifiedExample
                     (SrcSpan "test/assets/UnverifiedExamples.hs" 29 1 34 14)
                     [ "[ 1" , ", 2" , ", 3" , ", 4" , "] |> map (+ 1)" , "==> True" ])
              ]
          }
      ]
  }