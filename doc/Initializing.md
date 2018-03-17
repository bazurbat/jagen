### Initializing

```
Usage: init-project [OPTIONS...] [PRODUCT] [LAYERS...]
       init-project [-h]
       init-project [-l]

  Initializes current directory as Jagen project.

SYNOPSIS:

  The script will put an environment file 'env.sh' and a configuration file
  'config.sh' in the current directory. The environment file should be sourced
  into the working shell before issuing any other Jagen commands. The
  configuration file contains project-specific settings and is sourced by a
  generator and a build system.

  The list of project layers is constructed from non-option arguments. Each
  layer can contribute rule definitions and environment overrides to the
  project.

  The project directory can also be called "build root" for a family of
  packages. A layout for a typical build root is described below:

    /bin     -- generated helper scripts
    /build   -- build system logs and working directories
    /host    -- install root for 'host' configuration
    /include -- package-specific include scripts generated from rules
    /src     -- checked out sources for SCM packages
    /target  -- install root for 'target' configuration

  The layout can differ depending on the configuration. These directories can
  be removed and re-created during the normal operation according the commands
  given. It is not safe to store important data inside the build root.

  Mixing environments from different projects (sourcing env.sh into the same
  shell) is not supported.

OPTIONS:

  -a                   add flag
  -f                   use force
  -h                   show usage
  --help               show this help

  Source packages, software distributions, patches and toolchains are located
  inside the project by default.

  The 'init-project' command refuses to initialize non-empty directories by
  default. Use '-f' option to override the check.

  The generated environment binds the project to the corresponding jagen source
  directory. If one or the other is moved or sourced from different root from
  which it was originally initialized (like chroot or Docker container) any
  build-related command will likely produce wrong results.

  The generated configuration can be adjusted manually but will be overwritten
  by the next 'init-project' invocation. Use '-a' option to set 'jagen_flags'
  at the initialization time; it can be specified multiple times.

EXAMPLES:

    # assuming jagen is checked out into ~/work
    cd ~/work
    mkdir ast100
    cd ast100
    ../jagen/init-project ast100 -a flag1 -a flag2
    . ./env.sh
    jagen build
    exit

  For subsequent invocations:

    cd ~/work/ast100
    . ./env.sh
    jagen build -f target1 target2
```
