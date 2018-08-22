### Cleaning

```
Usage: jagen list [packages|layers] [OPTIONS...]

  Lists various information about the current project.

COMMANDS

  packages  List package rules and their origin

  The 'packages' command displays a list of all currently defined packages
  along with contexts where their definitions were found. Contexts could be
  rule files or other packages which mention given package as their requires.
  In the displayed filenames the parent directory of the current project is
  shown as '...'.

  packages options:

    --all, -a
        Show also implicit rules added by the generator such as the toolchain
        dependencies. These rules will be marked with "*".

    --depth,-d <level>
        Set the maximum depth of the rule contexts displayed. If none was
        specified the 0 is the default which results in showing only the
        toplevel packages explicitly defined in the rule files. If the option
        is given without a value it is set to 999 which means show all
        contexts.

  layers  Show currently defined layers and their file paths.
```
