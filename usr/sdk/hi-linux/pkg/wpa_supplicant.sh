#!/bin/sh

include_from 'vendor/ast'

jagen_pkg_configure_target() {
    rm -f .config

    echo "BINDIR=$pkg_install_prefix/bin"    >> .config

    echo "CONFIG_CTRL_IFACE=y"               >> .config
    echo "CONFIG_BACKEND=file"               >> .config

    echo "CONFIG_PEERKEY=y"                  >> .config

    echo "CONFIG_BGSCAN_SIMPLE=y"            >> .config
    echo "CONFIG_BGSCAN_LEARN=y"             >> .config

    echo "CONFIG_TLS=internal"               >> .config
    echo "CONFIG_INTERNAL_LIBTOMMATH=y"      >> .config
    echo "CONFIG_INTERNAL_LIBTOMMATH_FAST=y" >> .config

    echo "CONFIG_CTRL_IFACE_DBUS_NEW=y"      >> .config

    echo "CFLAGS += $(pkg-config --cflags libnl-3.0)" >> .config
    echo "LIBS   += $(pkg-config --libs libnl-3.0)"   >> .config
    echo "CONFIG_DRIVER_NL80211=y"           >> .config
    echo "CONFIG_LIBNL32=y"                  >> .config

    echo "CONFIG_DELAYED_MIC_ERROR_REPORT=y" >> .config

    echo "CONFIG_IEEE8021X_EAPOL=y"          >> .config
}
