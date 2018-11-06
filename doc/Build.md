## Building

`jagen build [OPTION...] [PATTERN...] [--] [TOOL OPTIONS...]`

Builds or rebuilds the specified targets.

### Options

Option             | Description
-------------------|------------
-h, --help         | print this help message
-m, --match        | print expanded value of target patterns and exit
-c, --clean        | clean package's build directories before the build
-i, --interactive  | build targets in interactive mode
-a, --all          | continue until everything is up to date
-n, --no-rebuild   | do not rebuild targets which are already up to date
-p, --progress     | show build progress

### Synopsis

The specified patterns are expanded and matching targets are rebuilt. Use the
'--match' option to print the matches without building anything.

Use the '--clean' option to remove the package's build directories before the
start. It also causes the 'configure' stage of the affected packages to become
out of date.

With the '--interactive' option the specified target commands will be run directly without going
through Jagen wrapper. This allows build tools to detect the terminal and show colored messages but
the output will not be captured and target dependencies will not be honored. This means that in the
most cases you will need to run the build without the '-i' option first to make sure all
requirements are up to date.

Use the '--all' option to build everything out of date in the current project
in addition to the specified targets.

The '--no-rebuild' option causes the command to behave similarly to 'make': it
ensures that targets are up to date instead of rebuilding them unconditionally.

The '--progress' option enables printing of the build progress from all logs in
parallel. If this option is not given the output is shown only for targets
directly specified on the command line.

The '--follow' option allows monitoring the output from build commands in real
time. Works best when used for a single package or dependent targets because
when there are several package builds in progress their output will be
intermixed.

The '--quiet' option disables build progress output from Jagen and leaves the
build tool (Ninja) connected directly to the terminal.

Arguments after '--' will be passed literally to the underlying build tool.
The handling depends on the build tool in question but be aware of the case
when multiple targets matched as the same arguments will be passed to all of
their build commands which might have surprising results.

It is best to use this functionality to quickly adjust the behaviour of a
single compile or configure command, such as:

    jagen build nanomsg:compile:host -- -v

to pass `-v` flag to Ninja to temporarily enable verbose mode, or, in the case
of 'make' build type:

    jagen build someppkg:compile:host -- V=1

Another possibility:

    jagen build nanomsg:configure:host -- -DSOME_FLAG=ON

to enable CMake flag `SOME_FLAG` without going to the build directory.
