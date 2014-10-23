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
             (after (xsdk unpack)
                    (rootfs build)))
     '(install))

(pkg 'yamon
     '(unpack (prereq check))
     '(prepare)
     '(build (after (xsdk unpack)
                    (rootfs build)))
     '(install))

; rootfs

(pkg 'rootfs
     '(unpack (prereq check))
     '(build (after (make install host)))
     '(install (kernel install)
               (loop-aes install)
               (mrua modules)
               (utils-linux install)
               (e2fsprogs install)
               (gnupg install)
               (dbus install)
               (libuv install)
               (utils install)
               (gdbserver install)
               (freetype install)
               (sqlite install)
               (strace install)
               (ralink install)
               (wpa_supplicant install)))
