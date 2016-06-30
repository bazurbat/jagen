#!/bin/sh

# Unfortunately vendor SDKs assume kernel sources placement with hardcoded
# relative paths, no luck with out of tree builds so far.
# export KBUILD_OUTPUT="${pkg_build_dir:?}"
