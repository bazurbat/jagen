#!/bin/sh

p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

encoders="pcm_s16le"

decoders="
aac
ac3
ass
cdgraphics
dca
flac
mjpeg
mp3float
pcm_s16le
srt
vorbis
"

muxers="wav"

demuxers="
ass
avi
cdg
flac
flv
image2
matroska
mjpeg
mov
mp3
mpegps
mpegts
ogg
srt
wav
"

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

protocols="file pipe tcp"

filters="afade aresample volume"

pkg_patch() {
    if [ "$pkg_sdk" = "hisilicon" ]; then
        sed -ri "s/^(SLIBNAME_WITH_MAJOR)='.*'$/\\1='\$(SLIBNAME)'/g" \
            "$p_source_dir/configure"
        sed -ri "s/^(SLIB_INSTALL_NAME)='.*'$/\\1='\$(SLIBNAME)'/g" \
            "$p_source_dir/configure"
        sed -ri "s/^(SLIB_INSTALL_LINKS)='.*'$/\\1=''/g" \
            "$p_source_dir/configure"
    fi
}

pkg_build() {
    local prefix cross_options

    CFLAGS=""
    CXXFLAGS=""

    if [ "$p_config" = "host" ]; then
        prefix="$host_dir"
    else
        prefix="$target_prefix"

        cross_options="--target-os=linux --enable-cross-compile"
        case $target_board in
            ast25|ast50|ast100)
                cross_options="$cross_options --cross-prefix=${toolchain_bin_dir}/${target_system}-"
                cross_options="$cross_options --arch=mipsel --cpu=24kf"
                ;;
            *)
                cross_options="$cross_options \
                    --cross-prefix=${android_toolchain_dir}/bin/${target_system}-"
                cross_options="$cross_options \
                    --sysroot=$android_toolchain_dir/sysroot"
                cross_options="$cross_options --arch=$target_arch"
                ;;
        esac
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
    for i in $demuxers; do
        components="$components --enable-demuxer=$i"
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
    case $pkg_build_type in
        Rel*) options="--disable-debug" ;;
        Debug)
            options="--disable-optimizations"
            components="$components --enable-decoder=eac3"
            ;;
    esac

    p_run $p_source_dir/configure --prefix="$prefix" \
        --bindir="${prefix}/bin" \
        --enable-gpl --enable-nonfree \
        --disable-static --enable-shared \
        --disable-runtime-cpudetect \
        --disable-programs \
        --disable-doc \
        --disable-avdevice \
        --disable-postproc \
        --disable-everything \
        $components \
        $cross_options \
        --extra-ldflags="-fPIC" \
        --enable-pic \
        --disable-symver \
        --disable-stripping \
        $options

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
