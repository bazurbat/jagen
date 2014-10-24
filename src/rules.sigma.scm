; prereq

(pkg 'prereq
     '(check))

(pkg 'xsdk
     '(unpack (prereq check)))

; host

(pkg 'make
     '(config host
              (unpack (prereq check))
              (build)
              (install)))

(pkg 'gdb
     '(config host
              (unpack (prereq check))
              (prepare)
              (build)
              (install)))

; utils

(pkg 'utils
     '(unpack (prereq check))
     '(config host
              (build)
              (install))
     '(build (gpgme install)
             (dbus install))
     '(install))

; boot

(pkg 'ezboot
     '(build (prereq check)
             after
             (xsdk unpack)
             (rootfs build))
     '(install))

(pkg 'yamon
     '(unpack (prereq check))
     '(prepare)
     '(build after
             (xsdk unpack)
             (rootfs build))
     '(install))

; rootfs

(pkg 'rootfs
     '(unpack (prereq check))
     '(build after
             (make install host))
     '(install (kernel install)
               (loop-aes install)
               (mrua modules)
               (util-linux install)
               (e2fsprogs install)
               (gnupg install)
               (dbus install)
               (rsync install)
               (ntpclient install)
               (libuv install)
               (utils install)
               (gdbserver install)
               (freetype install)
               (sqlite install)
               (strace install)
               (ralink install)
               (wpa_supplicant install)))

(pkg 'util-linux
     '(unpack (prereq check))
     '(prepare)
     '(build (rootfs build))
     '(install))

(pkg 'e2fsprogs
     '(unpack (prereq check))
     '(prepare)
     '(build (util-linux install))
     '(install))

(pkg 'libgpg-error
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

(pkg 'libassuan
     '(unpack (prereq check))
     '(build (libgpg-error install))
     '(install))

(pkg 'gpgme
     '(unpack (prereq check))
     '(build (libassuan install))
     '(install))

(pkg 'gunpg
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

(pkg 'expat
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

(pkg 'wpa_supplicant
     '(unpack)
     '(prepare)
     '(build (rootfs build)
             (dbus install))
     '(install))

; (pkg 'popt
;      '(unpack (prereq check))
;      '(build (rootfs build))
;      '(install))
;
; (pkg 'device-mapper
;      '(unpack (prereq check))
;      '(build (libgpg-error install))
;      '(install))
;
; (pkg 'libgcrypt
;      '(unpack (prereq check))
;      '(prepare)
;      '(build (rootfs build))
;      '(install))
;
; (pkg 'cryptsetup
;      '(unpack (prereq check))
;      '(prepare)
;      '(build (e2fsprogs install)
;              (popt install)
;              (device-mapper install)
;              (libgcrypt install))
;      '(install))

(pkg 'dbus
     '(unpack (prereq check))
     '(build (expat install))
     '(install))

(pkg 'rsync
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

(pkg 'ntpclient
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

(pkg 'libuv
     '(unpack (prereq check))
     '(prepare)
     '(build (rootfs build))
     '(install))

(pkg 'gdbserver
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

; (pkg 'oprofile
;      '(unpack (prereq check))
;      '(prepare)
;      '(build (popt install))
;      '(install))

(pkg 'freetype
     '(unpack (prereq check))
     '(prepare)
     '(build (rootfs build))
     '(install))

(pkg 'sqlite
     '(unpack (prereq check))
     '(prepare)
     '(build (rootfs build))
     '(install))

(pkg 'strace
     '(unpack (prereq check))
     '(build (rootfs build))
     '(install))

; (pkg 'chibi-scheme
;      '(config host
;               (unpack (prereq check))
;               (build)
;               (install))
;      '(unpack (prereq check))
;      '(patch)
;      '(build (chibi-scheme install host)
;              (rootfs build))
;      '(install))

(pkg 'kernel
     '(unpack (prereq check))
     '(build (rootfs build)
             after (xsdk unpack))
     '(install)
     '(image))

(pkg 'ralink
     '(unpack (prereq check))
     '(prepare)
     '(build (kernel build))
     '(install))

(pkg 'loop-aes
     '(unpack (prereq check))
     '(build (kernel build))
     '(install))

(pkg 'mrua
     '(build (kernel build))
     '(modules)
     '(install (firmware prepare)))

(pkg 'chicken
     '(unpack (prereq check))
     '(config host
              (build)
              (install))
     '(config cross
              (build after (chicken install host))
              (install))
     '(config target
              (build after (rootfs build) (chicken install cross))
              (install (firmware prepare))))

(pkg 'chicken-eggs
     '(unpack (prereq check))
     '(config host
              (install (chicken install host)))
     '(config cross
              (install (chicken install cross)
                       after (chicken-eggs install host)))
     '(config target
              (install (chicken install)
                       after
                       (chicken-eggs install cross)
                       (dbus install))))

(pkg 'ffmpeg
     '(unpack (prereq check))
     '(build after (rootfs build))
     '(install (firmware prepare))
     '(config host
              (build)
              (install)))

(pkg 'soundtouch
     '(unpack (prereq check))
     '(prepare)
     '(build after (rootfs build))
     '(install (firmware prepare)))

(pkg 'karaoke-player
     '(unpack (prereq check))
     '(build (mrua build)
             (ffmpeg install)
             (soundtouch install)
             (chicken install)
             (chicken-eggs install cross))
     '(install after (chicken eggs install))
     '(config host
              (build (ffmpeg build host)
                     (chicken-eggs install host))
              (install)))

(pkg 'firmware
     '(unpack (prereq check))
     '(prepare)
     '(material (mrua build))
     '(install (karaoke-player install)) ; files/firmware/fwversion.sexp
     '(clean (firmware material)
             (mrua install)))
