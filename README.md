# Example of combining Rust code into a Haskell library

This is a simple (hopefully mostly complete) example
of wrapping a Rust dynamic library into a Haskell library, which calls it, using only `Cabal`, which should make it possible to properly package such a library. 

The main 'trick' is the custom `Setup.hs` which invokes `cargo build` whenever necessary,
and to add cargo's output directory (which is `./target/release`, courtesy of using a Cargo workspace), as absolute file path, programmatically to the `extra-lib-dirs` of the created library.

Caveat emptor, this has not been thorougly tested:

- On non-Unix OSes
- When actually attempting to include such a library inside a final executable.

It might still contain some unhandled edge cases.
