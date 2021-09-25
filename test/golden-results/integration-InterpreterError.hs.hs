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

[4mNot all examples verified![m
Verified: 0
Unverified: 0
No examples: 0
Evaluation failed: 1
  In these files:
  * test/assets/InterpreterError.hs