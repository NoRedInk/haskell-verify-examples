ExampleVerifySuccess
  (VerifiedExample
     (SrcSpan "" (-1) (-1) (-1) (-1))
     [ "[ 1"
     , ", 2"
     , ", 3"
     , "]"
     , "|> List.map (+ 1)"
     , "==>"
     , "[ 2"
     , ", 3"
     , ", 5"
     , "]"
     ])
  (Unverified "[2,3,4]" "[2,3,5]")