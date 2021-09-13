ExampleVerifySuccess
  (VerifiedExample
     ( SrcSpanInfo
         { srcInfoSpan = SrcSpan "" (-1) (-1) (-1) (-1)
         , srcInfoPoints = []
         }
     , "[ 1\n, 2\n, 3\n]\n|> List.map (+ 1)\n==>\n[ 2\n, 3\n, 5\n]"
     ))
  (Unverified "[2,3,4]" "[2,3,5]")