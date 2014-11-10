
(define (define-rootfs-package name . deps)
  (pkg name
       `((clean)
         (unpack)
         (prepare)
         (build (rootfs build) ,@deps)
         (install))))

(define (define-kernel-package name . deps)
  (pkg name
       `((clean)
         (unpack)
         (prepare)
         (build (kernel build) ,@deps)
         (install))))

; base

(pkg 'ast-files
     '((clean)
       (unpack)))

(pkg 'linux
     '((clean)
       (unpack)))

(pkg 'xsdk
     '((clean)
       (unpack)))

(pkg 'ucode
     '((clean)
       (unpack)
       (install (firmware unpack))))

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

(define-rootfs-package 'ezboot)

(define-rootfs-package 'yamon)

; rootfs

(pkg 'rootfs
     '((clean)
       (unpack)
       (prepare)
       (build after
              (ast-files unpack)
              (xsdk unpack)
              (make install host))
       (install (kernel install)
                (dbus install)
                (e2fsprogs install)
                (freetype install)
                (gdbserver install)
                (gnupg install)
                (libuv install)
                (loop-aes install)
                (mrua modules)
                (ntpclient install)
                (ralink install)
                (rsync install)
                (sqlite install)
                (strace install)
                (util-linux install)
                (utils install target)
                (wpa_supplicant install))))

(define-rootfs-package 'expat)
(define-rootfs-package 'dbus '(expat install))
(define-rootfs-package 'wpa_supplicant '(dbus install))

(define-rootfs-package 'util-linux)
(define-rootfs-package 'e2fsprogs '(util-linux install))

(define-rootfs-package 'freetype)
(define-rootfs-package 'gdbserver)
(define-rootfs-package 'libuv)
(define-rootfs-package 'ntpclient)
(define-rootfs-package 'rsync)
(define-rootfs-package 'sqlite)
(define-rootfs-package 'strace)

; gpgme

(define-rootfs-package 'libgpg-error)
(define-rootfs-package 'libassuan '(libgpg-error install))
(define-rootfs-package 'gpgme '(libassuan install))

(define-rootfs-package 'gnupg)

; kernel

(pkg 'kernel
     '((clean)
       (unpack)
       (build (linux unpack)
              (rootfs build))
       (install)
       (image (rootfs install))))

(define-kernel-package 'ralink)

(define-kernel-package 'loop-aes)

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
     '((clean)
       (unpack)
       (material (mrua build))
       (install (karaoke-player install target)) ; files/firmware/fwversion.sexp
       (strip (mrua install))))
