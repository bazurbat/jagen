#!/bin/sh

export cmake_generator="${cmake_generator:-Ninja}"
export cmake_build_options="${cmake_build_options}"
export cmake_build_type="${cmake_build_type:-$pkg_build_type}"
