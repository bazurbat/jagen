set(CMAKE_SYSTEM_NAME "Linux")
set(CMAKE_FIND_ROOT_PATH "$ENV{pkg_install_dir}")

# Remove CMake's defaults which are appended to the generic flags and override
# our environment.
set(CMAKE_CXX_FLAGS_RELEASE "")
set(CMAKE_C_FLAGS_RELEASE "")
