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
module Test where 
    
-- Below is a verified example
-- > testFunction 1 ==> 2
testFunction :: Int -> Int
testFunction = (+1) 
```

How about a failing example? 

```haskell
module Test where 
    
-- Below is a verified example
-- > testFunction 1 ==> 3
testFunction :: Int -> Int
testFunction = (+1) 
```

You can also inquire about the value of an expression using `?`

```haskell
module Test where 

-- ? testFunc 1
testFunc :: Int -> Int
testFunc = (+1)
```

## Development

1. `$ direnv allow`
1. `$ make watch`
