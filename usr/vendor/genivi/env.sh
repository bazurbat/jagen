#!/bin/sh

export COMMONAPI_CONFIG="$jagen_host_dir/etc/commonapi.ini"
export COMMONAPI_DBUS_CONFIG="$jagen_host_dir/etc/commonapi-dbus.ini"

export PERS_CLIENT_DBUS_ADDRESS="${DBUS_SESSION_BUS_ADDRESS-}"
export PERS_CLIENT_LIB_CUSTOM_LOAD="$jagen_host_dir/etc/pclCustomLibConfigFile.cfg"
