### stack quick start instructions: https://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html

```haskell
stack build
stack exec scrabble-oracle-exe
```

equiv to

```haskell
stack install  -- synonym for `stack build --copy-bins`
-- this will tell you name of executable
-- and add exe to path so you can just call
scrabble-oracle-exe
```
