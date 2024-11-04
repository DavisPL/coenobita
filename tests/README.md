# Testing Coenobita

Testing Coenobita is challenging because it's a compiler plugin that involves complicated interactions between multiple crates. Following in the footsteps of the Rust compiler and related tools, we have set up UI tests for Coenobita using `trybuild`. Before reading any further, you should probably check out its [documentation](https://github.com/dtolnay/trybuild).

Instead of asserting the equality or inequality of values, we assert that a program either _passes_ or _fails_ with some specific message. All of our tests live in the `a/tests` directory. Right now, it only has an `integrity` subfolder, but there will likely be more in the future.

Every category (like integrity or provenance) has two additional folders inside of it named `pass` and `fail`. Like their names suggest, all the programs in `pass` should compile successfully and all the programs in `fail` should fail to compile. Not only should programs in the second group fail, but they must do so with a specific error message.

For example, consider the simple program in `a/tests/integrity/fail/fn/fn.0.rs`. It should fail to compile with the following message.

```
error: expected {b}{b}, found {a}{a}
 --> tests/integrity/fail/fn/fn.0.rs:6:5
  |
6 |     x
  |     ^

error: expected {a}{a}, found {bin}{bin}
  --> tests/integrity/fail/fn/fn.0.rs:10:16
   |
10 |     let x = id(5);
   |                ^
```

We make sure that it fails with this exact message by including it in `fn.0.stderr`.
