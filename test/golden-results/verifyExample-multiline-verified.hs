ExampleVerifySuccess
  (VerifiedExample
     ( SrcSpanInfo
         { srcInfoSpan = SrcSpan "" (-1) (-1) (-1) (-1)
         , srcInfoPoints = []
         }
     , [ "[ 1"
       , ", 2"
       , ", 3"
       , "]"
       , "|> List.map (+ 1)"
       , "==>"
       , "[ 2"
       , ", 3"
       , ", 4"
       , "]"
       ]
     ))
  Verified