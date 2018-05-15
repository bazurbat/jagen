## Rust Support

Jagen supports [Rust](https://www.rust-lang.org) packages natively including cross-compilation and
toolchain selection. Use build type `rust` to enable it, for example:
```
package { 'hello', 'host',
    build = 'rust'
}
```
This will download and initialize Rust environment using rustup.

... to be continued.
