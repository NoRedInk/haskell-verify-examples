Examples of module Simple unverified.

test/assets/Simple.hs:15
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

test/assets/UnverifiedExamples.hs:12
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


test/assets/UnverifiedExamples.hs:14
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


test/assets/UnverifiedExamples.hs:20
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


test/assets/UnverifiedExamples.hs:29
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

Examples of module InterpreterError unverified.

test/assets/InterpreterError.hs:7
[2m  2: 
[m[2m  3: import Prelude
[m[2m  4: 
[m[2m  5: -- TypeApplications
[m[2m  6: --
[m✗ 7: -- > test ==> testx
[2m  8: test :: Maybe [Char]
[m[2m  9: test = Just "a"
[m[2m  10: 
[mThe example doesn't compile:
<interactive>:2:10: error:
    • Variable not in scope: testx :: Maybe [Char]
    • Perhaps you meant ‘test’ (line 9)

Examples unverified.

test/assets/Headless.hs:3
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

Examples of module UnkownLanguageExtension unverified.

test/assets/UnknownLanguageExtensions.hs:8
[2m  3: module UnkownLanguageExtension where
[m[2m  4: 
[m[2m  5: import Prelude
[m[2m  6: 
[m[2m  7: -- TypeApplications
[m✗ 8: -- > id @(Maybe [Char]) test ==> (Just "a")
[2m  9: test :: Maybe [Char]
[m[2m  10: test = Just "a"
[m[2m  11: 
[mUnknown extensions:
* UnkownLanguageExtension

[4mNot all examples verified![m
Verified: 15
Unverified: 4
  In these files:
  * test/assets/Headless.hs
  * test/assets/UnverifiedExamples.hs
  * test/assets/WithContext.hs
No examples: 2
  In these files:
  * test/assets/Simple.hs
  * test/assets/UnverifiedExamples.hs
Evaluation failed: 3
  In these files:
  * test/assets/InterpreterError.hs
  * test/assets/UnknownLanguageExtensions.hs
  * test/assets/UnverifiedExamples.hs