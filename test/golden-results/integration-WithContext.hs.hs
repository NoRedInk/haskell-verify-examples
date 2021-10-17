Examples of module WithContext unverified.

test/assets/WithContext.hs:23
[2m  18: --
[m[2m  19: -- Examples:
[m[2m  20: --
[m[2m  21: -- > identity @(Maybe [Char]) test ==> result
[m[2m  22: --
[m✗ 23: -- > Just "b" ==> result
[2m  24: test :: Maybe [Char]
[m[2m  25: test = Just "a"
[m[2m  26: 
[m[2m  27: -- |
[m[2m  28: --
[mThe example was incorrect and couldn't be verified.
      ▼
Just "b"
╷
│ ==>
╵
Just "a"
      ▲


test/assets/WithContext.hs:31
[2m  26: 
[m[2m  27: -- |
[m[2m  28: --
[m[2m  29: -- Examples:
[m[2m  30: --
[m✗ 31: -- > testNoAccessToResult ==> result
[2m  32: testNoAccessToResult :: Maybe [Char]
[m[2m  33: testNoAccessToResult = Just "a"
[m[2m  34: 
[m[2m  35: -- |
[m[2m  36: --
[mThe example doesn't compile:
<interactive>:2:26: error:
    Variable not in scope: result :: Maybe [Char]

[4mNot all examples verified![m
Verified: 3
Unverified: 1
  In these files:
  * test/assets/WithContext.hs
No examples: 0
Evaluation failed: 1
  In these files:
  * test/assets/WithContext.hs