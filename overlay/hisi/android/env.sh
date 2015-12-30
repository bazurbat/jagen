#!/bin/sh

jagen_target_system="arm-hisiv200-linux"

jagen_target_product="Hi3719CV100"

jagen_target_toolchain_dir="${jagen_target_dir}/bin"

jagen_shell="/bin/bash"

if in_flags ccache; then
    export USE_CCACHE=1
fi
