
(define (define-rootfs-package name . deps)
  (pkg name
       `((clean)
         (unpack)
         (build (rootfs build) ,@deps)
         (install))))

(pkg 'ast-files
     '((clean)
       (unpack)))

; host

(pkg 'make
     '((clean)
       (unpack)
       (config host
               (build)
               (install))))

(pkg 'gdb
     '((clean)
       (unpack)
       (prepare)
       (config host
               (build)
               (install))))

; utils

(pkg 'utils
     '((clean)
       (unpack)
       (config host
               (build)
               (install))
       (config target
               (build (gpgme install)
                      (dbus install))
               (install))))

; boot

(pkg 'xsdk
     '((clean)
       (unpack)))

(pkg 'ucode
     '((clean)
       (unpack)
       (install)))

(pkg 'ezboot
     '((clean)
       (unpack)
       (build after (rootfs build))
       (install)))

(pkg 'yamon
     '((clean)
       (unpack)
       (prepare)
       (build after (rootfs build))
       (install)))

; rootfs

(pkg 'rootfs
     '((clean)
       (unpack)
       (prepare)
       (build after
              (xsdk unpack)
              (make install host))
       (install (kernel install)
                (loop-aes install)
                (mrua modules)
                (util-linux install)
                (e2fsprogs install)
                (gnupg install)
                (dbus install)
                (rsync install)
                (ntpclient install)
                (libuv install)
                (utils install target)
                (gdbserver install)
                (freetype install)
                (sqlite install)
                (strace install)
                (ralink install)
                (wpa_supplicant install))))

(pkg 'util-linux
     '((clean)
       (unpack)
       (prepare)
       (build (rootfs build))
       (install)))

(pkg 'e2fsprogs
     '((clean)
       (unpack)
       (prepare)
       (build (util-linux install))
       (install)))

(pkg 'libgpg-error
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

(pkg 'libassuan
     '((clean)
       (unpack)
       (build (libgpg-error install))
       (install)))

(pkg 'gpgme
     '((clean)
       (unpack)
       (build (libassuan install))
       (install)))

(pkg 'gnupg
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

(pkg 'expat
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

(pkg 'wpa_supplicant
     '((clean)
       (unpack)
       (prepare)
       (build (rootfs build)
              (dbus install))
       (install)))

; (pkg 'popt
;      '((clean)
;      (unpack)
;      (build (rootfs build))
;      (install)))
;
; (pkg 'device-mapper
;      '((clean)
;      (unpack)
;      (build (libgpg-error install))
;      (install)))
;
; (pkg 'libgcrypt
;      '((clean)
;      (unpack)
;      (prepare)
;      (build (rootfs build))
;      (install)))
;
; (pkg 'cryptsetup
;      '((clean)
;      (unpack)
;      (prepare)
;      (build (e2fsprogs install)
;              (popt install)
;              (device-mapper install)
;              (libgcrypt install))
;      (install)))

(pkg 'dbus
     '((clean)
       (unpack)
       (build (expat install))
       (install)))

(pkg 'rsync
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

(pkg 'ntpclient
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

(pkg 'libuv
     '((clean)
       (unpack)
       (prepare)
       (build (rootfs build))
       (install)))

(pkg 'gdbserver
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

; (pkg 'oprofile
;      '((clean)
;      (unpack)
;      (prepare)
;      (build (popt install))
;      (install)))

(pkg 'freetype
     '((clean)
       (unpack)
       (prepare)
       (build (rootfs build))
       (install)))

(pkg 'sqlite
     '((clean)
       (unpack)
       (prepare)
       (build (rootfs build))
       (install)))

(pkg 'strace
     '((clean)
       (unpack)
       (build (rootfs build))
       (install)))

; (pkg 'chibi-scheme
;      '((config host
;               (unpack)
;               (build)
;               (install))
;      (unpack)
;      (patch)
;      (build (chibi-scheme install host)
;              (rootfs build))
;      (install)))

(pkg 'linux
     '((clean)
       (unpack)))

(pkg 'kernel
     '((clean)
       (unpack)
       (build (linux unpack)
              (rootfs build))
       (install)
       (image (ast-files unpack))))

(pkg 'ralink
     '((clean)
       (unpack)
       (prepare)
       (build (kernel build))
       (install)))

(pkg 'loop-aes
     '((clean)
       (unpack)
       (build (kernel build))
       (install)))

(pkg 'mrua
     '((clean)
       (unpack)
       (build (kernel build))
       (modules)
       (install (firmware unpack))))

(pkg 'chicken
     '((clean)
       (unpack)
       (config host
               (build)
               (install))
       (config cross
               (build after (chicken install host))
               (install))
       (config target
               (build after (rootfs build) (chicken install cross))
               (install (firmware unpack)))))

(pkg 'chicken-eggs
     '((clean)
       (unpack)
       (config host
               (install (chicken install host)))
       (config cross
               (install (chicken install cross)
                        after (chicken-eggs install host)))
       (config target
               (install (chicken install target)
                        after
                        (chicken-eggs install cross)
                        (dbus install)))))

(pkg 'ffmpeg
     '((clean)
       (unpack)
       (config host
               (build (ast-files unpack))
               (install))
       (config target
               (build (ast-files unpack)
                      after (rootfs build))
               (install (firmware unpack)))))

(pkg 'soundtouch
     '((clean)
       (unpack)
       (prepare)
       (build after (rootfs build))
       (install (firmware unpack))))

(pkg 'astindex
     '((clean)
       (unpack (karaoke-player unpack))))

(pkg 'karaoke-player
     '((clean)
       (unpack)
       (config host
               (build (astindex unpack)
                      (ffmpeg build host)
                      (chicken-eggs install host))
               (install))
       (config target
               (prepare)
               (build (astindex unpack)
                      (mrua build)
                      (ffmpeg install target)
                      (soundtouch install)
                      (chicken install target)
                      (chicken-eggs install cross))
               (install after (chicken-eggs install target)))))

(pkg 'firmware
     '((unpack)
       (material (mrua build))
       (install (karaoke-player install target)) ; files/firmware/fwversion.sexp
       (clean (firmware material)
              (mrua install))))
