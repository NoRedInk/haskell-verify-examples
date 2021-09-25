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
[mThe example was incorrect and couldn't be verified.
        ▼
"Just \"b\""
╷
│ ==>
╵
"Just \"a\""
        ▲

[4mNot all examples verified![m
Verified: 1
Unverified: 1
  In these files:
  * test/assets/WithContext.hs
No examples: 0
Evaluation failed: 0