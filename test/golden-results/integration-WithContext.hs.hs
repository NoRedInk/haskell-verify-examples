Examples of module WithContext unverified.

test/assets/WithContext.hs:21
[2m  16: -- result = Just "a"
[m[2m  17: -- @
[m[2m  18: --
[m[2m  19: -- Examples:
[m[2m  20: --
[m✗ 21: -- > identity @(Maybe [Char]) test ==> result
[2m  22: --
[m[2m  23: -- > Just "b" ==> result
[m[2m  24: test :: Maybe [Char]
[m[2m  25: test = Just "a"
[m[2m  26: 
[mThe example doesn't compile:
/tmp/HaskellVerifiedExamples20303-1.hs:52:1: error:
    parse error on input ‘import’/tmp/HaskellVerifiedExamples20303-1.hs:52:1: error:
    parse error on input ‘import’


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
[mThe example doesn't compile:
/tmp/HaskellVerifiedExamples20303-1.hs:52:1: error:
    parse error on input ‘import’/tmp/HaskellVerifiedExamples20303-1.hs:52:1: error:
    parse error on input ‘import’


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


test/assets/WithContext.hs:48
[2m  43: -- result = Just "a"
[m[2m  44: -- @
[m[2m  45: --
[m[2m  46: -- Examples:
[m[2m  47: --
[m✗ 48: -- > test3 ==> result
[2m  49: test3 :: Maybe [Char]
[m[2m  50: test3 = Just "a"
[m[2m  51: 
[mThe example doesn't compile:
/tmp/HaskellVerifiedExamples20303-2.hs:52:1: error:
    parse error on input ‘import’/tmp/HaskellVerifiedExamples20303-2.hs:52:1: error:
    parse error on input ‘import’

[4mNot all examples verified![m
Verified: 0
Unverified: 0
No examples: 0
Evaluation failed: 4
  In these files:
  * test/assets/WithContext.hs