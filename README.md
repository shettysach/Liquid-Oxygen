## Liquid-Oxygen

A tree-walk interpreter for the Lox language, based on the first half of the book, Crafting Interpreters by Robert Nystorm.

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

- [Crafting Interpreters by Robert Nystorm](https://craftinginterpreters.com/) 

### Notes:

Will be optimized further.

#### Linked Lists 
- `[Char]` (`String`) is just a linked list of characters, which is inefficient.
- Alternatives like `Text` (from `text`) or `ByteString` (from `bytestring`) are much faster and more memory-efficient.
- Similarly, for other linked list operations, other structures like `Vector` or `Seq` can offer better performance depending on use case.

#### Resolving
- The original implementation uses a `locals` map to track only local variable resolutions.
- My implementation uses `distances`, which uniformly tracks local and global by their scope depth.

#### Initialize vs Assign
- Only `modifyIORef` (lazy) works for `initialize` due to Functions, because closures depend on lazily init envs and may involve cyclic refs (`mdo` / `fixIO`).
- `atomicModifyIORef` (strict) seems to work safely for `assignAt`, since assignment happens after eval and does not involve cyclic structs.
