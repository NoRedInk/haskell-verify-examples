Examples of module Simple unverified.

test/assets/passing/Simple.hs:15
[2m  10: -- > ==> 1
[m[2m  11: --
[m[2m  12: -- > test + test
[m[2m  13: -- > ==> 2
[m[2m  14: --
[m✗ 15: -- > test + test
[2m  16: test = 1
[m[2m  17: 
[mNo expected result for example.

Examples of module UnverifiedExamples unverified.

test/assets/passing/UnverifiedExamples.hs:12
[2m  7: -- hello world
[m[2m  8: --
[m[2m  9: -- > test
[m[2m  10: -- > ==> 1
[m[2m  11: --
[m✗ 12: -- > test
[2m  13: --
[m[2m  14: -- > test + test
[m[2m  15: -- > ==> 3
[m[2m  16: test = 1
[m[2m  17: 
[mNo expected result for example.


test/assets/passing/UnverifiedExamples.hs:14
[2m  9: -- > test
[m[2m  10: -- > ==> 1
[m[2m  11: --
[m[2m  12: -- > test
[m[2m  13: --
[m✗ 14: -- > test + test
[2m  15: -- > ==> 3
[m[2m  16: test = 1
[m[2m  17: 
[m[2m  18: -- | more stuff
[m[2m  19: --
[mThe example was incorrect and couldn't be verified.
 ▼
"2"
╷
│ ==>
╵
"3"
 ▲


test/assets/passing/UnverifiedExamples.hs:20
[2m  15: -- > ==> 3
[m[2m  16: test = 1
[m[2m  17: 
[m[2m  18: -- | more stuff
[m[2m  19: --
[m✗ 20: -- > [ 1
[2m  21: -- > , 2
[m[2m  22: -- > , 3
[m[2m  23: -- > , 4
[m[2m  24: -- > ] |> map (+ 1)
[m[2m  25: -- > ==> [ 2, 4, 5 ]
[mThe example was incorrect and couldn't be verified.
    ▼▼
"[2,3,4,5]"
╷
│ ==>
╵
"[2,4,5]"


test/assets/passing/UnverifiedExamples.hs:29
[2m  24: -- > ] |> map (+ 1)
[m[2m  25: -- > ==> [ 2, 4, 5 ]
[m[2m  26: 
[m[2m  27: -- | compilation fails
[m[2m  28: --
[m✗ 29: -- > [ 1
[2m  30: -- > , 2
[m[2m  31: -- > , 3
[m[2m  32: -- > , 4
[m[2m  33: -- > ] |> map (+ 1)
[m[2m  34: -- > ==> True
[mThe example doesn't compile:
<interactive>:7:5: error:
    • Couldn't match expected type ‘[value0]’ with actual type ‘Bool’
    • In the second argument of ‘(==>)’, namely ‘True’
      In the expression: [1, 2, 3, 4] |> map (+ 1) ==> True
      In an equation for ‘e_112341’:
          e_112341 = [1, 2, 3, ....] |> map (+ 1) ==> True

Examples of module WithContext unverified.

test/assets/passing/WithContext.hs:21
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

Examples unverified.

test/assets/passing/Headless.hs:3
[2m  -2: -- > 1 ==> 1
[m[2m  -1: --
[m[2m  0: -- > 1 ==> 2
[m[2m  1: testHeadless = 1
[m[2m  2: 
[mThe example was incorrect and couldn't be verified.
 ▼
"1"
╷
│ ==>
╵
"2"
 ▲

[4mNot all examples verified![m
Verified: 15
Unverified: 4
  In these files:
  * test/assets/passing/Headless.hs
  * test/assets/passing/UnverifiedExamples.hs
  * test/assets/passing/WithContext.hs
No examples: 2
  In these files:
  * test/assets/passing/Simple.hs
  * test/assets/passing/UnverifiedExamples.hs
Evaluation failed: 1
  In these files:
  * test/assets/passing/UnverifiedExamples.hs