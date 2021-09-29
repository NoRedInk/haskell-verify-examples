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
[m[2m  18: -- > test2 + test2 ==> 4
[m[2m  19: test2 = 2
[m[2m  20: 
[mThe example doesn't compile:
<interactive>:2:8: error:
    • Couldn't match expected type ‘integer-gmp-1.0.3.0:GHC.Integer.Type.Integer
                                    -> integer-gmp-1.0.3.0:GHC.Integer.Type.Integer’
                  with actual type ‘integer-gmp-1.0.3.0:GHC.Integer.Type.Integer’
    • The function ‘test’ is applied to one argument,
      but its type ‘integer-gmp-1.0.3.0:GHC.Integer.Type.Integer’
      has none
      In the second argument of ‘(+)’, namely ‘test test2’
      In the first argument of ‘(+)’, namely ‘test + test test2’

[4mNot all examples verified![m
Verified: 2
Unverified: 0
No examples: 0
Evaluation failed: 1
  In these files:
  * test/assets/Simple.hs