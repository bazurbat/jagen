(define (define-rootfs-package name . deps)
  (pkg name
       `(build (rootfs build) ,@deps)
       '(install)))

(define (define-kernel-package name . deps)
  (pkg name
       `(build (kernel build) ,@deps)
       '(install)))

(define (define-firmware-package name . deps)
  (pkg name
       `(build ,@deps after (rootfs build))
       '(install (firmware unpack))))

; base

(pkg 'ast-files)

(pkg 'linux)

(pkg 'xsdk)

(pkg 'ucode
     '(install (firmware unpack)))

; host

(pkg 'make
     '(config host
              (build)
              (install)))

; utils

(pkg 'utils
     '(config host
              (build)
              (install))
     '(config target
              (build (gpgme install)
                     (dbus install))
              (install)))

; boot

(define-rootfs-package 'ezboot)

;(define-rootfs-package 'yamon)

; debugging

(when (string=? "Debug" (env 'build-type))
  (pkg 'gdb
       '(config host
                (build)
                (install)))
  (define-rootfs-package 'gdbserver)
  (define-rootfs-package 'strace))

; rootfs

(pkg 'rootfs
     '(build after
             (ast-files unpack)
             (xsdk unpack)
             (make install host))
     '(install (kernel install)
               (gnupg install)
               (loop-aes install)
               (mrua modules)
               (ntpclient install)
               (ralink install)
               (util-linux install)
               (utils install target)))

(define-rootfs-package 'ntpclient)
(define-rootfs-package 'util-linux)

; gpgme

(define-rootfs-package 'libgpg-error)
(define-rootfs-package 'libassuan '(libgpg-error install))
(define-rootfs-package 'gpgme '(libassuan install))

(define-rootfs-package 'gnupg)

; kernel

(pkg 'kernel
     '(build (linux unpack)
             (ezboot build)
             (rootfs build))
     '(install)
     '(image (rootfs install)))

(define-kernel-package 'ralink)

(define-kernel-package 'loop-aes)

(pkg 'mrua
     '(build (kernel build))
     '(modules)
     '(install (firmware unpack)))

(pkg 'chicken
     '(config host
              (build)
              (install))
     '(config cross
              (build after (chicken install host))
              (install))
     '(config target
              (build after (rootfs build) (chicken install cross))
              (install (firmware unpack))))

(pkg 'chicken-eggs
     '(config host
              (install (chicken install host)))
     '(config cross
              (install (chicken install cross)
                       after (chicken-eggs install host)))
     '(config target
              (install (chicken install target)
                       after
                       (chicken-eggs install cross)
                       (dbus install))))

(define-firmware-package 'dbus '(expat install))
(define-firmware-package 'expat)
(define-firmware-package 'freetype)
(define-firmware-package 'libuv)
(define-firmware-package 'rsync)
(define-firmware-package 'sqlite)
(define-firmware-package 'wpa_supplicant '(dbus install))

(define-firmware-package 'xtables)
(define-firmware-package 'xtables-addons '(xtables install))

(pkg 'ffmpeg
     '(config host
              (build (ast-files unpack))
              (install))
     '(config target
              (build (ast-files unpack)
                     after (rootfs build))
              (install (firmware unpack))))

(pkg 'soundtouch
     '(build after (rootfs build))
     '(install (firmware unpack)))

(pkg 'astindex
     '(unpack (karaoke-player unpack)))

(pkg 'karaoke-player
     '(config host
              (build (astindex unpack)
                     (ffmpeg build host)
                     (chicken-eggs install host))
              (install))
     `(config target
              (prepare)
              (build (astindex unpack)
                     (chicken install target)
                     (chicken-eggs install cross)
                     (dbus install)
                     (ffmpeg install target)
                     (freetype install)
                     (libuv install)
                     (mrua build)
                     (soundtouch install))
              (install after (chicken-eggs install target))))

(pkg 'firmware
     '(material (mrua build))
     '(install (ezboot install)
               (kernel image)
               (karaoke-player install target))
     '(strip (mrua install)))

(when (regexp-search "jemalloc" *flags*)
  (pkg 'jemalloc
       '(build after (rootfs build))
       '(install (firmware unpack))))

(when (regexp-search "experimental_network" *flags*)
  (pkg 'libffi
       '(build after (rootfs build))
       '(install (firmware unpack)))

  (pkg 'glib
       '(build (libffi install))
       '(install (firmware unpack)))

  (pkg 'connman
       '(build (dbus install)
               (glib install)
               (xtables-addons install))
       '(install (firmware unpack))))
