#!/bin/sh

jagen_pkg_install() {
    local dst="$pkg_install_dir/xload"

    pkg_run install -vd "$dst"

    pkg_run install -vm 644 \
        "audio_microcode_t3iptv_prod_0x10_nodts.xload" \
        "$dst/audio_microcode_tango3.xload"
    pkg_run install -vm 644 \
        "video_microcode_t3iptv_prod_noreal.xload" \
        "$dst/video_microcode_tango3.xload"
    pkg_run install -vm 644 \
        "demuxpsf_microcode_t3iptv_prod.xload" \
        "$dst/demuxpsf_microcode_tango3.xload"
    pkg_run install -vm 644 \
        "ios.bin.gz_t3iptv_prod.xload" \
        "$dst/ios.bin_tango3.xload"
}
