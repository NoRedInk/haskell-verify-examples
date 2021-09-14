Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "WithContext"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 1 1 17 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/WithContext.hs" 1 1 1 1
                  , SrcSpan "test/assets/WithContext.hs" 2 1 2 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 6 1 6 1
                  , SrcSpan "test/assets/WithContext.hs" 15 1 15 1
                  , SrcSpan "test/assets/WithContext.hs" 16 1 16 1
                  , SrcSpan "test/assets/WithContext.hs" 17 1 17 1
                  , SrcSpan "test/assets/WithContext.hs" 17 1 17 1
                  ]
              }
        , languageExtensions = [ "TypeApplications" , "NoImplicitPrelude" ]
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
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 8 1 9 3
              , srcInfoPoints = []
              }
          , " TypeApplications\n"
          )
      , ContextBlockComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 10 1 12 5
              , srcInfoPoints = []
              }
          , "\n result = Just \"a\"\n"
          )
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 13 1 13 3
              , srcInfoPoints = []
              }
          , ""
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 14 1 14 40
                 , srcInfoPoints = []
                 }
             , "id @(Maybe [Char]) test ==> result"
             ))
      ]
  }