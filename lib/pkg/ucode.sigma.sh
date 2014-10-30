#!/bin/sh

case $sdkver in
    3.9*)  p_source="$p_dist_dir/mruafw_SMP8654F_prod_3_9_2.tgz" ;;
    3.11*) p_source="$p_dist_dir/mruafw_SMP8654F_3_11_3_prod.tgz" ;;
    4.0)   p_source="$p_dist_dir/mruafw_SMP8670F_prod_4_0_0.tgz" ;;
esac

pkg_install() {
    local dst="$sdk_firmware_dir/xload"

    p_run install -vd "$dst"

    p_run install -vm 644 \
        "audio_microcode_t3iptv_prod_0x10_nodts.xload" \
        "$dst/audio_microcode_tango3.xload"
    p_run install -vm 644 \
        "video_microcode_t3iptv_prod_noreal.xload" \
        "$dst/video_microcode_tango3.xload"
    p_run install -vm 644 \
        "demuxpsf_microcode_t3iptv_prod.xload" \
        "$dst/demuxpsf_microcode_tango3.xload"
    p_run install -vm 644 \
        "ios.bin.gz_t3iptv_prod.xload" \
        "$dst/ios.bin_tango3.xload"
}
