Examples of module WithContext unverified.

test/assets/WithContext.hs:21
[2m  16: --
[m[2m  17: -- Examples:
[m[2m  18: --
[m[2m  19: -- > identity @(Maybe [Char]) test ==> result
[m[2m  20: --
[m✗ 21: -- > Just "b" ==> result
[2m  22: test :: Maybe [Char]
[m[2m  23: test = Just "a"
[m[2m  24: 
[m[2m  25: -- | TypeApplications
[m[2m  26: --
[mThe example was incorrect and couldn't be verified.
        ▼
"Just \"b\""
╷
│ ==>
╵
"Just \"a\""
        ▲


test/assets/WithContext.hs:29
[2m  24: 
[m[2m  25: -- | TypeApplications
[m[2m  26: --
[m[2m  27: -- Examples:
[m[2m  28: --
[m✗ 29: -- > testNoAccessToResult ==> result
[2m  30: testNoAccessToResult :: Maybe [Char]
[m[2m  31: testNoAccessToResult = Just "a"
[m[2m  32: 
[mThe example doesn't compile:
<interactive>:2:26: error:
    Variable not in scope: result :: Maybe [Char]

[4mNot all examples verified![m
Verified: 1
Unverified: 1
  In these files:
  * test/assets/WithContext.hs
No examples: 0
Evaluation failed: 1
  In these files:
  * test/assets/WithContext.hs