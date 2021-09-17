Module
  { moduleInfo =
      ModuleInfo
        { moduleName = Just "WithContext"
        , moduleSource =
            SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 1 1 20 1
              , srcInfoPoints =
                  [ SrcSpan "test/assets/WithContext.hs" 1 1 1 1
                  , SrcSpan "test/assets/WithContext.hs" 2 1 2 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 4 1 4 1
                  , SrcSpan "test/assets/WithContext.hs" 6 1 6 1
                  , SrcSpan "test/assets/WithContext.hs" 18 1 18 1
                  , SrcSpan "test/assets/WithContext.hs" 19 1 19 1
                  , SrcSpan "test/assets/WithContext.hs" 20 1 20 1
                  , SrcSpan "test/assets/WithContext.hs" 20 1 20 1
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
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 10 1 15 5
              , srcInfoPoints = []
              }
          , "\n import Prelude\n\n result :: Maybe [Char]\n result = Just \"a\"\n"
          )
      , PlainTextComment
          ( SrcSpanInfo
              { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 16 1 16 3
              , srcInfoPoints = []
              }
          , ""
          )
      , CodeBlockComment
          (VerifiedExample
             ( SrcSpanInfo
                 { srcInfoSpan = SrcSpan "test/assets/WithContext.hs" 17 1 17 40
                 , srcInfoPoints = []
                 }
             , "id @(Maybe [Char]) test ==> result"
             ))
      ]
  }