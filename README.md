Lox interpreter.
Learning Haskell and Crafting Interpreters.

- Work in progress
- Scoping errors

### Cabal
```sh
cabal run liquid-oxygen <script>
```

### GHC only

#### Build

- bash / zsh
  ```sh
  ghc -o lox src/Main.hs $(ls src/ | grep -v "Main.hs" | sed 's|^|src/|') -outputdir /tmp -O3
  ```

- nushell
  ```nushell
  ghc -o lox src/Main.hs ...(ls src/ | get name | filter {|x| $x != "src/Main.hs"}) -outputdir /tmp -O3
  ```

Be sure to clean `/tmp` later.

#### Run
```sh
./lox <script>
```

Resources
- https://craftinginterpreters.com/
