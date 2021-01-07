#!/bin/sh
#shellcheck disable=2154,2155

pkg__get_toolchain() {
    local suffix=$1 default=$2 value
    value=$(eval echo "\$pkg_build_$suffix")
    if [ "$value" != "${value#*/}" ]; then
        echo "$value"
    else
        echo "${pkg_toolchain_prefix}${value:-$default}"
    fi
}

# CMake honors CC and CXX too but not LD. It uses the compiler for linking.
export  CC=$(pkg__get_toolchain cc  gcc)
export CXX=$(pkg__get_toolchain cxx g++)
export  LD=$(pkg__get_toolchain ld  ld )

# Some of those are not very standard but relatively common.
export AR="${pkg_toolchain_prefix}ar"
export AS="${pkg_toolchain_prefix}as"
export CPP="${pkg_toolchain_prefix}cpp"
export NM="${pkg_toolchain_prefix}nm"
export OBJCOPY="${pkg_toolchain_prefix}objcopy"
export OBJDUMP="${pkg_toolchain_prefix}objdump"
export RANLIB="${pkg_toolchain_prefix}ranlib"
export STRIP="${pkg_toolchain_prefix}strip"

export jagen_pkg__cflags="$toolchain_cflags"
export jagen_pkg__cxxflags="$toolchain_cxxflags"
export jagen_pkg__ldflags="$toolchain_ldflags"

if [ "$pkg_build_without_toolchain_cflags" ] || [ "$pkg_build_unset_cflags" ]; then
    jagen_pkg__cflags=
    jagen_pkg__cxxflags=
    jagen_pkg__ldflags=
fi

# Some configure scripts try to link libraries by name (-lfoo) when probing
# dependencies and fail to find the libraries in the staging directory if no
# such library is installed on the host system or produce incorrect results by
# finding a different version. By passing extra options to a toolchain wrapper
# we avoid overriding the package prefferred default CFLAGS and LDFLAGS if any.

if [ "$pkg_build_with_install_dir" ] || [ "$pkg_build_configure_needs_install_dir" ]; then
    jagen_pkg__cflags="${jagen_pkg__cflags} -I${pkg_install_dir}/include"
    jagen_pkg__cxxflags="${jagen_pkg__cxxflags} -I${pkg_install_dir}/include"
    jagen_pkg__ldflags="${jagen_pkg__ldflags} -L${pkg_install_dir}/lib"
fi

# Do not let user's global env interfere with the build root.
unset CFLAGS CXXFLAGS LDFLAGS

case $pkg_build_type in
cmake) ;; # pkg-specific build flags for CMake are handled by configure
*) [ "$pkg_build_cflags"   ] && export CFLAGS="$pkg_build_cflags"
   [ "$pkg_build_cxxflags" ] && export CXXFLAGS="$pkg_build_cxxflags"
   [ "$pkg_build_ldflags"  ] && export LDFLAGS="$pkg_build_ldflags" ;;
esac

if [ "$pkg_build_toolchain" ]; then
    PATH="${jagen_bin_dir}/${pkg_build_toolchain}:$PATH"
fi

return 0
