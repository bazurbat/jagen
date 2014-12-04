(define (define-rootfs-package name source . deps)
  (pkg name
       source
       (stages `(build (rootfs build) ,@deps)
               '(install))))

(define (define-kernel-package name source . deps)
  (pkg name
       source
       (stages `(build (kernel build) ,@deps)
               '(install))))

(define (define-firmware-package name source . deps)
  (pkg name
       source
       (stages `(build ,@deps)
               '(install (firmware unpack)))))

; base

(pkg 'ast-files
     (source 'git "git@bitbucket.org:art-system/files.git"))

(pkg 'linux
     (source 'git "git@bitbucket.org:art-system/linux.git"))

(pkg 'xsdk
     (source 'dist "${cpukeys}.tar.gz"))

(pkg 'ucode
     (source 'dist "mruafw_SMP8654F_prod_3_9_2.tgz")
     (stages '(install (firmware unpack))))

; tools

(pkg 'make
     (source 'dist "make-3.80.tar.bz2")
     (stages '(config host
                      (build)
                      (install))))

; host

(pkg 'libtool
     (source 'dist "libtool-2.4.3.tar.xz")
     (stages '(build)
             '(install)))

; utils

(pkg 'utils
     (source 'git "git@bitbucket.org:art-system/sigma-utils.git")
     (stages '(config host
                      (build)
                      (install))
             '(config target
                      (build (gpgme install)
                             (dbus install))
                      (install))))

; boot

(define-rootfs-package
  'ezboot
  (source 'git "git@bitbucket.org:art-system/sigma-ezboot.git"))

;(define-rootfs-package 'yamon)

; debugging

(when (string=? "Debug" (env 'build-type))
  (pkg 'gdb
       (stages '(config host
                        (build)
                        (install))))
  (define-rootfs-package
    'gdbserver
    (source 'dist "gdb-7.6.2.tar.bz2"))
  (define-rootfs-package
    'strace
    (source 'dist "strace-4.8.tar.xz")))

; rootfs

(pkg 'rootfs
     (source 'git "git@bitbucket.org:art-system/sigma-rootfs.git")
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

(define-rootfs-package
  'busybox
  (source 'dist "busybox-1.22.1.tar.bz2"))
(define-rootfs-package
  'ntpclient
  (source 'dist "ntpclient-2010.tar.gz"))
(define-rootfs-package
  'util-linux
  (source 'dist "util-linux-2.23.2.tar.xz"))

; gpgme

(define-rootfs-package
  'libgpg-error
  (source 'dist "libgpg-error-1.17.tar.bz2"))

(define-rootfs-package
  'libassuan
  (source 'dist "libassuan-2.1.2.tar.bz2")
  '(libgpg-error install))

(define-rootfs-package
  'gpgme
  (source 'dist "gpgme-1.5.1.tar.bz2")
  '(libassuan install))

(define-rootfs-package
  'gnupg
  (source 'dist "gnupg-1.4.18.tar.bz2"))

; kernel

(pkg 'kernel
     (source 'git "git@bitbucket.org:art-system/sigma-kernel.git")
     (stages '(build (linux unpack)
                     (ezboot build)
                     (rootfs build))
             '(install)
             '(image (rootfs install))))

(define-kernel-package
  'ralink
  (source 'dist "DPO_RT5572_LinuxSTA_2.6.1.3_20121022.tar.bz2"))

(define-kernel-package
  'loop-aes
  (source 'dist "loop-AES-v3.7a.tar.bz2"))

(pkg 'mrua
     (source 'git "git@bitbucket.org:art-system/sigma-mrua.git")
     (stages '(build (kernel build))
             '(modules)
             '(install (firmware unpack))))

(pkg 'chicken
     (source 'git "https://github.com/bazurbat/chicken-scheme.git")
     (stages '(config host
                      (build)
                      (install))
             '(config target
                      (build (chicken install host))
                      (install (firmware unpack)))))

(pkg 'chicken-eggs
     (source 'git "https://github.com/bazurbat/chicken-eggs.git")
     (stages '(config host
                      (install (chicken install host)))
             '(config target
                      (install (chicken install target)
                               after
                               (chicken-eggs install host)
                               (dbus install)))))

(define-firmware-package
  'dbus
  (source 'dist "dbus-1.6.18.tar.gz")
  '(expat install))

(define-firmware-package
  'expat
  (source 'dist "expat-2.1.0.tar.gz"))

(define-firmware-package
  'freetype
  (source 'dist "freetype-2.5.0.1.tar.bz2"))

(define-firmware-package
  'libuv
  (source 'dist "libuv-v0.10.25.tar.gz"))

(define-firmware-package
  'rsync
  (source 'dist "rsync-3.1.1.tar.gz"))

(define-firmware-package
  'sqlite
  (source 'dist "sqlite-autoconf-3080403.tar.gz"))

(define-firmware-package
  'wpa_supplicant
  (source 'dist "wpa_supplicant-2.2.tar.gz")
  '(dbus install))

(define-firmware-package
  'zlib
  (source 'dist "zlib-1.2.8.tar.gz"))

(define-firmware-package
  'xtables
  (source 'dist "iptables-1.4.21.tar.bz2"))

(define-firmware-package
  'xtables-addons
  (source 'dist "xtables-addons-1.47.1.tar.xz")
  '(xtables install))

(pkg 'ffmpeg
     (source 'dist "ffmpeg-2.2.1.tar.bz2")
     (stages '(config host
                      (build (ast-files unpack))
                      (install))
             '(config target
                      (build (ast-files unpack))
                      (install (firmware unpack)))))

(pkg 'soundtouch
     (source 'dist "soundtouch-1.8.0.tar.gz")
     (stages '(build)
             '(install (firmware unpack))))

(pkg 'astindex
     (source 'hg "ssh://hg@bitbucket.org/art-system/astindex")
     (stages '(unpack (karaoke-player unpack))))

(pkg 'karaoke-player
     (source 'hg "ssh://hg@bitbucket.org/art-system/karaoke-player")
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
       (source 'dist "jemalloc-3.6.0.tar.bz2")
       (stages '(build)
               '(install (firmware unpack)))))

(when (regexp-search "experimental_network" *flags*)
  (pkg 'libffi
       (source 'dist "libffi-3.1.tar.gz")
       (stages '(build (libtool install))
               '(install (firmware unpack))))

  (pkg 'glib
       (source 'dist "glib-2.40.2.tar.xz")
       (stages '(patch (libtool install))
               '(build (zlib install)
                       (libffi install))
               '(install (firmware unpack))))

  (pkg 'connman
       (source 'dist "connman-1.26.tar.xz")
       (stages '(build (libtool install)
                       (dbus install)
                       (glib install)
                       (xtables-addons install))
               '(install (firmware unpack)))))
