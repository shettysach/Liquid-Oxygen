## liquid-oxygen

A tree-walk interpreter for the Lox language, based on the first half of Crafting Interpreters.

### Cabal

#### REPL

```sh
cabal run liquid-oxygen
```

#### Interpret script

```sh
cabal run liquid-oxygen <script>
```

### Resources

- [Crafting Interpreters by Bob Nystorm](https://craftinginterpreters.com/) 

> **NOTES:**

> #### Map vs HashMap
> - Implementation - `HashMap` `(amortized O(1))` would be faster than `Map` `(O(log n))` for variable lookup since variable scopes don't need ordering, but I chose `Map` because `HashMap` is external (`unordered-containers`), whereas `Map` is in base/containers. 
> - Might change this later, as it seems like a drop-in replacement.*


> #### Resolving
> - The original implementation uses a `locals` map to track only local variable resolutions.
> - My implementation uses `distances`, which uniformly tracks local and global by their scope depth.

> #### Initialize vs Assign
> - Only `modifyIORef` (lazy) works for `initialize` due to Functions (example: `closure <- lift (initialize name fun env)`), because closures depend on lazily init envs and may involve cyclic refs (`mdo` / `fixIO`).
> - `atomicModifyIORef` (strict) seems to work safely for `assignAt`, since assignment happens after eval and does not involve cyclic structs.

#### Why the name?

https://en.wikipedia.org/wiki/Liquid_oxygen
