## Rust

Jagen supports packages written in [Rust](https://www.rust-lang.org) language including
cross-compilation. It transparently setups `rustup`, installs toolchains and adds targets as
needed. It adds convenience on top of bare `rustup` if you need to build multiple Rust packages for
different target systems, express dependencies from native (C/C++) packages or quickly initialize
CI environment.

To enable Rust support for a package set its build type to `rust`:

```lua
package { 'hello',
    build = 'rust'
}
```

By default `stable` Rust toolchain is assumed for the package which will be downloaded and cached
in `$jagen_dist_dir/rustup` directory during the first build. To change the toolchain use the
`rust_toolchain` build property:

```lua
package { 'hello',
    build = {
        type = 'rust',
        rust_toolchain = '1.26.2'
    }
}
```

A given name will be passed to `rustup toolchain install` command if the toolchain is not already
installed so anything this command accepts can be used as the value. For each distinct
`<rust_toolchain>` value the package `rust-<rust_toolchain>` will be declared and added to the
project, for example: `rust-stable` (the default) or `rust-1.26.2`. You can use this package name
to reinstall the toolchain manually but this normally is not needed because it will be pulled by
`rust` packages as a dependency.

When changing the toolchain for already compiled package a clean rebuild is necessary to recompile
the package using the new toolchain, use `--clean` argument for the "build" command, for example:

```
jagen build --clean hello
```

To change the target system for the package use the `system` property, such as
`x86_64-unknown-linux-musl` to have fully static binaries.

```lua
package { 'hello',
    build = {
        type = 'rust',
        rust_toolchain = '1.26.2',
        system = 'x86_64-unknown-linux-musl'
    }
}
```

You can specify any target from `rustup target list` but generally non-native targets will fail to
link if the system compiler is not able handle the needed binary format. Set the `toolchain`
property to specify the cross-compiler which will be used to link the target binaries.

```lua
package { 'hello',
    build = {
        type = 'rust',
        toolchain = 'android-standalone-arm'
    }
}
```

Note that the `system` property is not necessary in this case because it will be derived from the
toolchain package.

The definitions for the following toolchains are supplied with Jagen itself and can be used without
additional configuration:

    android-standalone-arm
    linaro-aarch64-5.3
    linaro-arm-4.8
    linaro-arm-4.9
    linaro-arm-5.4
    linaro-arm-6.4
    linaro-arm-7.2
    sourcery-mips-2012.03

The specified toolchain will be added to the package dependencies, downloaded and unpacked
automatically during the build.

For each distinct Rust toolchain name and target (specified as a `system` or derived from a
toolchain`) a separate `rust-\*` package will be added to the project, such as:
`rust-1.26.2-x86_64-unknown-linux-musl` or `rust-stable-android-standalone`. This way you can have
different packages using different Rust toolchains and targets in the same project.

The `rust` type packages are built out of source with `$CARGO\_TARGET\_DIR` set to
`$pkg\_build\_dir` which defaults to `$jagen\_build\_dir/<name>/<config>` which will be
`build/hello/host` directory in the current project for the examples above.

Each project has a separate `$RUSTUP\_HOME` (which defaults to `$jagen\_dist\_dir/rustup` and
`$CARGO\_HOME` (which defaults to `$jagen\_dist\_dir/cargo`). When the `rustup` package is
installed it is linked to `host/bin` directory which is added to `$PATH` by default, so you can use
`rustup` and other Cargo commands manually to manage Rust environment if the project-specific
`env.sh` is sourced in the current Shell.
