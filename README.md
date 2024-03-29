# UNDER CONSTRUCTION 🚧


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

```
Ok [(VerifiedExample (SrcSpan "/private/tmp/DocModule8657-23.hs" 4 1 4 26) ["testFunction 1 ==> 2"],ExampleVerifySuccess Verified)]
```


How about a failing example? 

```haskell
module Test where 
    
-- Below is a verified example
-- > testFunction 1 ==> 3
testFunction :: Int -> Int
testFunction = (+1) 
```

```
Ok [(VerifiedExample (SrcSpan "/private/tmp/DocModule8657-24.hs" 4 1 4 26) ["testFunction 1 ==> 3"],ExampleVerifySuccess (Unverified "2" "3"))]
```


You can also inquire about the value of an expression using `?`

```haskell
module Test where 

-- ? testFunc 1
testFunc :: Int -> Int
testFunc = (+1)
```

```
Ok [(VerifiedExample (SrcSpan "/private/tmp/DocModule8657-25.hs" 3 1 3 16) ["evaluteExampleTodo (","testFunc 1",")"],ExampleVerifySuccess (HelpTodo "2"))]
```


Code blocks can be introduced using the '@' operator.  These blocks are useful to demonstrate example usage. 

```haskell
module Test where

-- @
-- import Test 
--
-- x :: Int
-- x = testFunc $ testFunc $ testFunc 0
-- @
-- 
-- > x ==> 3
testFunc :: Int -> Int
testFunc = (+1)
```

```
Ok [(VerifiedExample (SrcSpan "/private/tmp/DocModule8657-26.hs" 10 1 10 13) ["x ==> 3"],ExampleVerifySuccess Verified)]
```


## Development

1. `$ direnv allow`
1. `$ make watch`
