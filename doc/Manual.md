# User Manual

## Contents

- [Initializing](Initializing.md)
- [List Information](List.md)
- [Building](Building.md)
- [Cleaning](Cleaning.md)
- [Managing package sources](ManagingSources.md)
- [Manage filesystem images](Images.md)
- [Install Bash completions](Installation.md)
- [Rules](Rules.md)
- [Build System](BuildSystem.md)
- [Rust Support](Rust.md)

### Introduction

A workflow is organized around "projects" which are composed from several
layers. Each layer can contribute rule definitions and environment variables to
the project. These definitions are evaluated to generate meta build system
which is then used to track package dependencies, allows selective rebuild,
cleanup and management of VCS sources.

Every rule, environment file or script can be overridden in the subsequent
layers or by the project itself. A library of built-in functions is provided to
ease integration of packages using common build systems like autotools and
CMake with their own set of workarounds. There are several vendor-specific
layers in the Jagen distribution which can be used as an example.

In short, it's a fresh remix of ideas from [Repo][], [BitBake][] and
[Buildroot][] with focus on speed and ease of use. Can be considered a much
simpler and a lighter-weight alternative to [OpenEmbedded][]/[Yocto][].

  [Repo]: https://source.android.com/source/using-repo.html
  [BitBake]: https://en.wikipedia.org/wiki/BitBake
  [Buildroot]: https://buildroot.org
  [Yocto]: https://www.yoctoproject.org/
  [OpenEmbedded]: http://www.openembedded.org
