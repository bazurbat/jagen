return {
    source = {
        type      = 'dist',
        location  = 'https://www.kernel.org/pub/linux/libs/security/linux-privs/libcap2/libcap-2.25.tar.xz',
        sha256sum = '693c8ac51e983ee678205571ef272439d83afe62dd8e424ea14ad9790bc35162',
    },
    patches = {
        -- taken from Gentoo
        { 'libcap-2.25-build-system-fixes', 1 },
        { 'libcap-2.22-no-perl', 1 },
        { 'libcap-2.25-ignore-RAISE_SETFCAP-install-failures', 1 },
        { 'libcap-2.21-include', 0 },
    },
    build = {
        type = 'make',
        in_source = true,
        options = {
            'BUILD_CC=gcc'
        }
    }
}
