# Haskell-Verify-Examples

Verify examples in your docs.

⚠️ This is not a replacement for tests, this tool should be used for improving your documentation.

## Install
1. `$ direnv allow`
2. `$ make install-exe`

## Setup
haskell-verify-examples will try to load the cradle automatically from the cwd to locate source files and project wide flags, so it should just work with cabal and stack projects.

## Writing Verified Examples

```haskell
-- Below is a verified example
-- > testFunction 1 ==> 2
-- 
-- If the left hand size and the r
testFunction :: Int -> Int
testFunction = (+1) 
```

```
Ok [(VerifiedExample (SrcSpan "/private/tmp/DocModule73855-0.hs" 2 1 2 26) ["testFunction 1 ==> 2"],ExampleVerifyFailed (InterpreterError (WontCompile [GhcError {errMsg = "<interactive>:2:1: error:\n    Variable not in scope: testFunction :: t0 -> a0"}])))]
```


## Development

1. `$ direnv allow`
1. `$ make watch`