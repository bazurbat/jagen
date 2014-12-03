(define (define-rootfs-package name . deps)
  (pkg name
       (stages `(build (rootfs build) ,@deps)
               '(install))))

(define (define-kernel-package name . deps)
  (pkg name
       (stages `(build (kernel build) ,@deps)
               '(install))))

(define (define-firmware-package name . deps)
  (pkg name
       (stages `(build ,@deps)
               '(install (firmware unpack)))))

; base

(pkg 'ast-files
     (source "git" "git@bitbucket.org:art-system/files.git"))

(pkg 'linux
     (source "git" "git@bitbucket.org:art-system/linux.git"))

(pkg 'xsdk)

(pkg 'ucode
     (stages '(install (firmware unpack))))

; host

(pkg 'make
     (source #f "$pkg_dist_dir/make-3.80.tar.bz2")
     (stages '(config host
                      (build)
                      (install))))

; utils

(pkg 'utils
     (source "git" "git@bitbucket.org:art-system/sigma-utils.git")
     (stages '(config host
                      (build)
                      (install))
             '(config target
                      (build (gpgme install)
                             (dbus install))
                      (install))))

; boot

(define-rootfs-package 'ezboot)

;(define-rootfs-package 'yamon)

; debugging

(when (string=? "Debug" (env 'build-type))
  (pkg 'gdb
       (stages '(config host
                        (build)
                        (install))))
  (define-rootfs-package 'gdbserver)
  (define-rootfs-package 'strace))

; rootfs

(pkg 'rootfs
     (source "git" "git@bitbucket.org:art-system/sigma-rootfs.git")
     (stages '(build after
                     (ast-files unpack)
                     (xsdk unpack)
                     (make install host))
             '(install (kernel install)
                       (busybox install)
                       (gnupg install)
                       (loop-aes install)
                       (mrua modules)
                       (ntpclient install)
                       (ralink install)
                       (util-linux install)
                       (utils install target))))

(define-rootfs-package 'busybox)
(define-rootfs-package 'ntpclient)
(define-rootfs-package 'util-linux)

; gpgme

(define-rootfs-package 'libgpg-error)
(define-rootfs-package 'libassuan '(libgpg-error install))
(define-rootfs-package 'gpgme '(libassuan install))

(define-rootfs-package 'gnupg)

; kernel

(pkg 'kernel
     (source "git" "git@bitbucket.org:art-system/sigma-kernel.git")
     (stages '(build (linux unpack)
                     (ezboot build)
                     (rootfs build))
             '(install)
             '(image (rootfs install))))

(define-kernel-package 'ralink)

(define-kernel-package 'loop-aes)

(pkg 'mrua
     (source "git" "git@bitbucket.org:art-system/sigma-mrua.git")
     (stages '(build (kernel build))
             '(modules)
             '(install (firmware unpack))))

(pkg 'chicken
     (source "git" "https://github.com/bazurbat/chicken-scheme.git")
     (stages '(config host
                      (build)
                      (install))
             '(config target
                      (build (chicken install host))
                      (install (firmware unpack)))))

(pkg 'chicken-eggs
     (source "git" "https://github.com/bazurbat/chicken-eggs.git")
     (stages '(config host
                      (install (chicken install host)))
             '(config target
                      (install (chicken install target)
                               after
                               (chicken-eggs install host)
                               (dbus install)))))

(define-firmware-package 'dbus '(expat install))
(define-firmware-package 'expat)
(define-firmware-package 'freetype)
(define-firmware-package 'libuv)
(define-firmware-package 'rsync)
(define-firmware-package 'sqlite)
(define-firmware-package 'wpa_supplicant '(dbus install))
(define-firmware-package 'zlib)

(define-firmware-package 'xtables)
(define-firmware-package 'xtables-addons '(xtables install))

(pkg 'ffmpeg
     (stages '(config host
                      (build (ast-files unpack))
                      (install))
             '(config target
                      (build (ast-files unpack))
                      (install (firmware unpack)))))

(pkg 'soundtouch
     (stages '(build)
             '(install (firmware unpack))))

(pkg 'astindex
     (source "hg" "ssh://hg@bitbucket.org/art-system/astindex")
     (stages '(unpack (karaoke-player unpack))))

(pkg 'karaoke-player
     (source "hg" "ssh://hg@bitbucket.org/art-system/karaoke-player")
     (stages '(config host
                      (build (astindex unpack)
                             (ffmpeg build host)
                             (chicken-eggs install host))
                      (install))
             `(config target
                      (prepare)
                      (build (astindex unpack)
                             (chicken install target)
                             (chicken-eggs install host)
                             (dbus install)
                             (ffmpeg install target)
                             (freetype install)
                             (libuv install)
                             (mrua build)
                             (soundtouch install)
                             ,@(if (regexp-search "experimental_network" *flags*)
                                 '((connman install))
                                 '()))
                      (install after (chicken-eggs install target)))))

(pkg 'firmware
     (stages '(material (mrua build))
             `(install (ezboot install)
                       (mrua install)
                       (kernel image)
                       (karaoke-player install target)
                       (dbus install)
                       (expat install)
                       (freetype install)
                       (libuv install)
                       (rsync install)
                       (sqlite install)
                       (wpa_supplicant install)
                       (zlib install)
                       ,@(if (regexp-search "experimental_network" *flags*)
                           '((libffi install)
                             (glib install)
                             (connman install))
                           '()))
             '(strip)))

(when (regexp-search "jemalloc" *flags*)
  (pkg 'jemalloc
       (stages '(build)
               '(install (firmware unpack)))))

(when (regexp-search "experimental_network" *flags*)
  (pkg 'libffi
       (stages '(build)
               '(install (firmware unpack))))

  (pkg 'glib
       (stages '(build (zlib install)
                       (libffi install))
               '(install (firmware unpack))))

  (pkg 'connman
       (stages '(build (dbus install)
                       (glib install)
                       (xtables-addons install))
               '(install (firmware unpack)))))
