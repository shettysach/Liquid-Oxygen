Lox interpreter.
Learning Haskell and Crafting Interpreters.

### Cabal

#### REPL

```sh
cabal run liquid-oxygen
```

#### Interpret script

```sh
cabal run liquid-oxygen <script>
```

Resources
- https://craftinginterpreters.com/ 

> TODO:
> - clock() - native function 

> **NOTES:**
> - In this version, the resolver also takes into account global variables by seeding the scope stack with `[Map.empty]`.
> - Only `modifyIORef` (lazy) works for `initialize` due to Functions (example: `closure <- lift (initialize name fun env)`), 
because closures depend on lazily init envs and may involve cyclic refs (`mdo` / `fixIO`).
> - `atomicModifyIORef` (strict) seems to work safely for `assignAt`, since assignment happens after eval and does not involve cyclic structs.
