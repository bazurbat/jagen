; prereq

(build prereq
       (check))

(build xsdk
       (unpack (prereq check)))

; host
(build make
       (config host
               (unpack (prereq check))
               (build (make unpack host))
               (install (make build host))))

(build gdb
       (config host
               (unpack (prereq check))
               (prepare (gdb unpack host))
               (build (gdb prepare host))
               (install (gdb build host))))

; utils

(build utils
       (unpack (prereq check))
       (config host
               (build (utils unpack))
               (install (utils build host)))
       (build (utils unpack)
              (gpgme install)
              (dbus install))
       (install (utils build)))

; boot

(build ezboot
       (build (prereq check)
              (after (xsdk unpack)
                     (rootfs build)))
       (install (ezboot build)))

(build yamon
       (unpack (prereq check))
       (prepare (yamon unpack))
       (build (yamon prepare)
              (after (xsdk unpack)
                     (rootfs build)))
       (install (yamon build)))

; rootfs

(build rootfs
       (unpack (prereq check))
       (build (rootfs unpack)
              (after (make install host)))
       (install (kernel install)
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
