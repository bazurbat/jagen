#!/bin/sh

encoders="pcm_s16le"

decoders="
aac
ac3
ass
cdgraphics
dca
flac
mjpeg
mp2float
mp3float
pcm_s16le
srt
vorbis
"

muxers="wav"

parsers="
aac
ac3
dca
flac
mjpeg
mpegaudio
mpegvideo
"

bsfs="h264_mp4toannexb"

protocols="file pipe rtp tcp"

filters="afade aresample volume"

jagen_pkg_patch() {
    # TODO: check if this is required
    # if [ "$jagen_sdk" = "hisilicon" ]; then
    #     sed -ri "s/^(SLIBNAME_WITH_MAJOR)='.*'$/\\1='\$(SLIBNAME)'/g" \
    #         "$pkg_source_dir/configure"
    #     sed -ri "s/^(SLIB_INSTALL_NAME)='.*'$/\\1='\$(SLIBNAME)'/g" \
    #         "$pkg_source_dir/configure"
    #     sed -ri "s/^(SLIB_INSTALL_LINKS)='.*'$/\\1=''/g" \
    #         "$pkg_source_dir/configure"
    # fi
    :
}

jagen_pkg_configure() {
    local cross_options

    CFLAGS=''
    CXXFLAGS=''

    if [ "$pkg_config" = "target" ]; then
        cross_options="--target-os=linux --enable-cross-compile"
        if [ "$jagen_toolchain_prefix" ]; then
            cross_options="$cross_options --cross-prefix=$jagen_toolchain_prefix"
        fi
        if [ "$jagen_toolchain_sysroot" ]; then
            cross_options="$cross_options --sysroot=$jagen_toolchain_sysroot"
        fi
        if [ "$jagen_target_arch" ]; then
            cross_options="$cross_options --arch=$jagen_target_arch"
        fi
        if [ "$jagen_target_cpu" ]; then
            cross_options="$cross_options --cpu=$jagen_target_cpu"
        fi
    fi

    local components=""

    for i in $encoders; do
        components="$components --enable-encoder=$i"
    done
    for i in $decoders; do
        components="$components --enable-decoder=$i"
    done
    for i in $muxers; do
        components="$components --enable-muxer=$i"
    done
    for i in $parsers; do
        components="$components --enable-parser=$i"
    done
    for i in $bsfs; do
        components="$components --enable-bsf=$i"
    done
    for i in $protocols; do
        components="$components --enable-protocol=$i"
    done
    for i in $filters; do
        components="$components --enable-filter=$i"
    done

    local options=""
    case $jagen_build_type in
        Rel*) options="--disable-debug" ;;
        Debug)
            options="--disable-optimizations"
            components="$components --enable-decoder=eac3"
            ;;
    esac

    pkg_run $pkg_source_dir/configure \
        --prefix="$pkg_prefix" \
        --enable-gpl --enable-nonfree \
        --disable-static --enable-shared \
        --disable-runtime-cpudetect \
        --disable-programs \
        --disable-doc \
        --disable-avdevice \
        --disable-postproc \
        --disable-everything \
        --enable-demuxers \
        $components \
        $cross_options \
        --extra-ldflags="-fPIC" \
        --enable-pic \
        --disable-symver \
        --disable-stripping \
        $options

    pkg_run make
}
