### Install Bash completions

  1. Make sure you are sourcing `bash_completion` in your `.bashrc` (necessary
     in Ubunty, not necessary in Gentoo).

```
     source "/usr/share/bash-completion/bash_completion"
```

  2. Add contents of `<jagen_dir>/misc/bash_completion` to `~/.bash_completion`
     file (or `~/.config/bash_completion` in Gentoo).

Completions defined for `jagen` command will work only when environment is
sourced from build root. Freeform target patterns such as `::host` or
`:install:` are not currently completed.
