## Rust Support

Jagen supports [Rust](https://www.rust-lang.org) natively. Set the build type to `rust` to enable
it for the package, for example:

```lua
package { 'hello',
    build = 'rust'
}
```

This will download and initialize Rust environment using rustup for the current project. After that
the packages itself will be built.

By default the toolchain from "stable" channel is used. You can override this by specifying
`rust_toolchain` property, like so:

```lua
package { 'hello',
    build = {
        type = 'rust',
        rust_toolchain = '1.24.0'
    }
}
```

where `1.24.0` is the name of the toolchain as would be given for `rustup install` command.

When changing the toolchain for already compiled package clean rebuild is necessary to recompile
the package using the new toolchain, use `--clean` argument for the "build" command, for example:

```
jagen build --clean hello
```

_Note: cross-compiling for Rust is work in progress._

It is also possible to generate binaries for not native targets (cross-compiled). Use the "target"
package configuration for that and specify the target toolchain which should be used to link the
executables. For example, to compile for Android, use:

```lua
package { 'hello', 'target',
    build = {
        type = 'rust',
        toolchain = 'android-standalone-arm'
    }
}
```

If using other targets the toolchain prefix might be different from Rust's canonical target name.
In this case specify Rust target name as a `system` build property in additional to a toolchain.
The prefix for the toolchain executables will still be determined from the toolchain definition.
```lua
package { 'hello', 'target',
    build = {
        type = 'rust',
        system = 'arm-unknown-linux-gnueabi',
        toolchain = 'linaro-arm-5.4'
    }
}
```
