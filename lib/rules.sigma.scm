
(define (define-rootfs-package name . deps)
  (pkg name
       `(build (rootfs build) ,@deps)
       '(install)))

(define (define-kernel-package name . deps)
  (pkg name
       `(build (kernel build) ,@deps)
       '(install)))

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

(define-rootfs-package 'yamon)

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
     `(install ,@(if (string=? "Debug" (env 'build-type))
                   '((gdbserver install)
                     (strace install))
                   '())
               (kernel install)
               (dbus install)
               (e2fsprogs install)
               (freetype install)
               (gnupg install)
               (libuv install)
               (loop-aes install)
               (mrua modules)
               (ntpclient install)
               (ralink install)
               (rsync install)
               (sqlite install)
               (util-linux install)
               (utils install target)
               (wpa_supplicant install)))

(define-rootfs-package 'expat)
(define-rootfs-package 'dbus '(expat install))
(define-rootfs-package 'wpa_supplicant '(dbus install))

(define-rootfs-package 'util-linux)
(define-rootfs-package 'e2fsprogs '(util-linux install))

(define-rootfs-package 'freetype)
(define-rootfs-package 'libuv)
(define-rootfs-package 'ntpclient)
(define-rootfs-package 'rsync)
(define-rootfs-package 'sqlite)

; gpgme

(define-rootfs-package 'libgpg-error)
(define-rootfs-package 'libassuan '(libgpg-error install))
(define-rootfs-package 'gpgme '(libassuan install))

(define-rootfs-package 'gnupg)

; kernel

(pkg 'kernel
     '(build (linux unpack)
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
     '(config target
              (prepare)
              (build (astindex unpack)
                     (mrua build)
                     (ffmpeg install target)
                     (libuv install)
                     (soundtouch install)
                     (chicken install target)
                     (chicken-eggs install cross))
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

  (pkg 'iptables
       '(build after (rootfs build))
       '(install (firmware unpack)))

  (pkg 'xtables-addons
       '(build (iptables install))
       '(install (firmware unpack)))

  (pkg 'connman
       '(build (xtables-addons install)
               (glib install))
       '(install (firmware unpack))))
