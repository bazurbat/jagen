#!/bin/sh

jagen_pkg_patch() {
    local config="$(find_file config/busybox.config)"

    if [ -f "$config" ]; then
        pkg_run cp -vf "$config" .config
    else
        die "Could not find BusyBox config for current configuration"
    fi
}

jagen_pkg_compile() {
    pkg_run make oldconfig
    pkg_run make
}

jagen_pkg_install() {
    pkg_run make CONFIG_PREFIX="$pkg_install_dir" install
}

jagen_pkg_deploy() {
    local dest="${1:?}"

    rsync -a --files-from=- "$pkg_install_dir/bin" "$dest/bin" <<'EOF' || die
[
[[
ash
awk
base64
basename
bunzip2
busybox
bzcat
cat
catv
chgrp
chmod
chown
chpst
chrt
clear
cp
cut
date
dd
df
dirname
dmesg
dnsdomainname
du
echo
egrep
env
envdir
expr
false
fgrep
find
free
fsync
fuser
grep
groups
gunzip
gzip
head
hostid
hostname
id
ionice
iostat
kill
less
ln
logger
login
logname
ls
lsof
md5sum
mkdir
mknod
mktemp
mount
mountpoint
mpstat
mv
netstat
nice
nmeter
nslookup
pgrep
pidof
ping
pipe_progress
pkill
pmap
printf
ps
pstree
pwd
pwdx
readlink
realpath
renice
reset
resize
rm
rmdir
run-parts
runsv
runsvdir
sed
seq
setserial
sh
sleep
smemcap
sort
stat
stty
sum
sv
sync
tail
tar
tee
test
time
timeout
top
touch
tr
true
umount
uname
uniq
unxz
uptime
usleep
vi
watch
wc
wget
which
xargs
xz
xzcat
yes
zcat
EOF

    rsync -a --files-from=- "$pkg_install_dir/sbin" "$dest/sbin" <<'EOF' || die
adjtimex
arping
crond
getty
halt
hwclock
ifconfig
ifdown
ifup
init
inotifyd
insmod
klogd
logread
lsmod
mdev
modinfo
modprobe
poweroff
readahead
reboot
rmmod
route
start-stop-daemon
svlogd
sysctl
syslogd
udhcpc
watchdog
EOF
}
