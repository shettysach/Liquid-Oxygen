Lox interpreter.
Learning Haskell and Crafting Interpreters.

**Build**

- bash / zsh
  ```bash
  ghc -o lox src/Main.hs \ 
  $(ls src/ | grep -v "Main.hs" | sed 's|^|src/|') \
  -O3 -outputdir /tmp
  ```

- nushell
  ```nushell
  ghc -o lox src/Main.hs ...(ls src/ |
  get name |
  filter {|x| $x != "src/Main.hs"}) -O3 -outputdir /tmp
  ```

**Run**
```sh
./lox <script>
```

Resources
- https://craftinginterpreters.com/
