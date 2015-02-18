#!/bin/sh

p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

pkg_build() {
    local prefix cross_options

    if [ "$p_config" = "host" ]; then
        prefix="$host_dir"
    else
        prefix="$target_prefix"
        cross_options="--enable-cross-compile --cross-prefix=$target_system- \
            --target-os=linux --arch=mipsel --cpu=24kf"
    fi

    local options
    local encoders decoders muxers protocols bsfs

    for i in aac pcm_s16le; do
        encoders="$encoders --enable-encoder=$i"
    done

    for i in $(cat "$pkg_private_dir/cfg/ffmpeg_audio_codecs.txt") cdgraphics hevc; do
        decoders="$decoders --enable-decoder=$i"
    done

	for i in adts pcm_s16le wav; do
		muxers="$muxers --enable-muxer=$i"
	done

    for i in file pipe; do
        protocols="$protocols --enable-protocol=$i"
    done

    for i in h264_mp4toannexb; do
        bsfs="$bsfs --enable-bsf=$i"
    done

    for i in $(cat "$pkg_private_dir/cfg/ffmpeg_filters.txt"); do
        filters="$filters --enable-filter=$i"
    done

    case $pkg_build_type in
        Rel*) options="--disable-debug" ;;
        Debug)
            export CFLAGS=""
            options="--disable-optimizations"
            ;;
    esac

    p_run $p_source_dir/configure --prefix="$prefix" \
        --bindir="${prefix}/bin" \
        --enable-gpl --enable-nonfree \
        --disable-static --enable-shared --disable-runtime-cpudetect \
        --disable-programs --enable-ffmpeg --enable-ffprobe --disable-avdevice \
        --disable-postproc --disable-doc --disable-htmlpages \
        --disable-manpages --disable-podpages --disable-txtpages \
        --disable-everything \
        --disable-encoders $encoders \
        --disable-decoders $decoders \
        --disable-hwaccels \
        --disable-muxers $muxers \
        --enable-demuxers \
        --enable-parsers \
        --disable-bsfs $bsfs \
        --disable-protocols $protocols \
        --disable-indevs --disable-outdevs --disable-devices \
        --disable-filters $filters \
        $cross_options \
        --disable-symver --disable-safe-bitstream-reader \
        --disable-stripping $options

    p_run make
}

pkg_build_host() {
    pkg_build
}

pkg_build_target() {
    pkg_build
}

pkg_install_host() {
    p_run make install
}

pkg_install_target() {
    p_run make DESTDIR="$target_dir" install
}
