# jagen

A straightforward build system generator.

Aimed for cases when OpenEmbedded is too magical and hard to configure for
obscure vendor SDK of choice. Intended to ease the pain of development of
multiple interdependent software packages with non standard toolchains,
cross-compilation and complex build dependencies. Based on ideas from Gentoo
Portage, GNU Guix and Nix package managers.

## Requirements

POSIX compatible shell, Lua 5.1.
