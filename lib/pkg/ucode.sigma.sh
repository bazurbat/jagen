#!/bin/sh

case $sdkver in
    3.9*)  psource="mruafw_SMP8654F_prod_3_9_2" ;;
    3.11*) psource="mruafw_SMP8654F_3_11_3_prod" ;;
    4.0)   psource="mruafw_SMP8670F_prod_4_0_0" ;;
esac

pkg_install() {
    local dst="$firmwaredir/xload"

    p_cmd install -vd "$dst"

    p_cmd install -vm 644 \
        "audio_microcode_t3iptv_prod_0x10_nodts.xload" \
        "$dst/audio_microcode_tango3.xload"
    p_cmd install -vm 644 \
        "video_microcode_t3iptv_prod_noreal.xload" \
        "$dst/video_microcode_tango3.xload"
    p_cmd install -vm 644 \
        "demuxpsf_microcode_t3iptv_prod.xload" \
        "$dst/demuxpsf_microcode_tango3.xload"
    p_cmd install -vm 644 \
        "ios.bin.gz_t3iptv_prod.xload" \
        "$dst/ios.bin_tango3.xload"
}
