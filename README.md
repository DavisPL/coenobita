# Coenobita

## Examples
Run the snippets provided in `examples` with `cargo run --example <filename_without_extension>`.

## What are capabilities?
**Capabilities** are unforgeable tokens that represent resources and the actions their holders may take.

Coenobita implements capabilities by replacing `Path` with `Cap<A1, A2, A3>` and `PathBuf` with `CapBuf<A1, A2, A3>`. The three generic arguments represent actions allowed on the resource, the resource's immediate children, and _all_ of the resource's children, respectively.

## Notes on Formatting
### `Cap` versus `CapBuf`
Because capabilities should directly correspond to paths, we provide two distinct capability types. `Cap` corresponds to `Path` and `CapBuf` corresponds to `PathBuf`. Thus, one may refer to a `CapBuf` as an _owned_ capability and a `Cap` as a capability _slice_.

### Generic Type Parameters
Some functions may use more than one capability, so it's important to choose generic type parameter labels that minimize confusion. We've chosen to use one letter for every unique capability used in an implementation or function.

For example, if a function accepts one `CapBuf` argument, it will have generic type parameters `A1`, `A2`, and `A3`. If the function were to accept a second `CapBuf`, it would have generic type parameters `B1`, `B2`, and `B3`.
