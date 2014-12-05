(define (define-rootfs-package name . args)
  (apply pkg name
         (stage "build" (depends '(rootfs build)))
         (stage "install")
         args))

(define (define-kernel-package name . args)
  (apply pkg name
         (stage "build" (depends '(kernel build)))
         (stage "install")
         args))

(define (define-firmware-package name . args)
  (apply pkg name
         (stage "build")
         (stage "install" (depends '(firmware unpack)))
         args))

; base

(pkg "ast-files"
     (source 'git "git@bitbucket.org:art-system/files.git"))

(pkg "linux"
     (source 'git "git@bitbucket.org:art-system/linux.git"))

(pkg "xsdk"
     (source 'dist "${cpukeys}.tar.gz"))

(pkg "ucode"
     (source 'dist "mruafw_SMP8654F_prod_3_9_2.tgz")
     (stage "install" (depends '(firmware unpack))))

; tools

(pkg "make"
     (source 'dist "make-3.80.tar.bz2")
     (stage "host" "build")
     (stage "host" "install"))

; host

(pkg "libtool"
     (source 'dist "libtool-2.4.3.tar.xz")
     (patch "libtool-2.4.3-no-clean-gnulib" 1)
     (patch "libtool-2.4.3-test-cmdline_wrap" 1)
     (stage "build")
     (stage "install"))

; utils

(pkg "utils"
     (source 'git "git@bitbucket.org:art-system/sigma-utils.git")
     (stage "host" "build")
     (stage "host" "install")
     (stage "target" "build" (depends '(gpgme install)
                                      '(dbus install)))
     (stage "target" "install"))

; boot

(define-rootfs-package
  "ezboot"
  (source 'git "git@bitbucket.org:art-system/sigma-ezboot.git"))

; fails to build with new toolchain
;(define-rootfs-package "yamon")

; debugging

(when (string=? "Debug" (env 'build-type))
  (pkg "gdb"
       (source 'dist "gdb-7.6.2.tar.bz2")
       (patch "05_all_readline-headers" 1)
       (patch "05_all_readline-headers" 1)
       (patch "15_all_gdb-7.6-btrace" 1)
       (stage "host" "build")
       (stage "host" "install"))

  (define-rootfs-package
    "gdbserver"
    (source 'dist "gdb-7.6.2.tar.bz2"))

  (define-rootfs-package
    "strace"
    (source 'dist "strace-4.8.tar.xz")))

; rootfs

(pkg "rootfs"
     (source 'git "git@bitbucket.org:art-system/sigma-rootfs.git")
     (stage "build" (after '(ast-files unpack)
                           '(xsdk unpack)
                           '(make install host)))
     (stage "install" (depends '(kernel install)
                               '(busybox install)
                               '(gnupg install)
                               '(loop-aes install)
                               '(mrua modules)
                               '(ntpclient install)
                               '(ralink install)
                               '(util-linux install)
                               '(utils install target))))

(define-rootfs-package
  "busybox"
  (source 'dist "busybox-1.22.1.tar.bz2"))

(define-rootfs-package
  "ntpclient"
  (source 'dist "ntpclient-2010.tar.gz"))

(define-rootfs-package
  "util-linux"
  (source 'dist "util-linux-2.23.2.tar.xz")
  (patch "util-linux-2.23.2" 1))

; gpgme

(define-rootfs-package
  "libgpg-error"
  (source 'dist "libgpg-error-1.17.tar.bz2"))

(define-rootfs-package
  "libassuan"
  (source 'dist "libassuan-2.1.2.tar.bz2")
  (stage "build" (depends '(libgpg-error install))))

(define-rootfs-package
  "gpgme"
  (source 'dist "gpgme-1.5.1.tar.bz2")
  (stage "build" (depends '(libassuan install))))

(define-rootfs-package
  "gnupg"
  (source 'dist "gnupg-1.4.18.tar.bz2"))

; kernel

(pkg "kernel"
     (source 'git "git@bitbucket.org:art-system/sigma-kernel.git")
     (stage "build" (depends '(linux unpack)
                             '(ezboot build)
                             '(rootfs build)))
     (stage "install")
     (stage "image" (depends '(rootfs install))))

(define-kernel-package
  "ralink"
  (source 'dist "DPO_RT5572_LinuxSTA_2.6.1.3_20121022.tar.bz2")
  (patch "DPO_RT5572_LinuxSTA_2.6.1.3_20121022-no-tftpboot" 1)
  (patch "DPO_RT5572_LinuxSTA_2.6.1.3_20121022-encrypt" 1))

(define-kernel-package
  "loop-aes"
  (source 'dist "loop-AES-v3.7a.tar.bz2"))

(pkg "mrua"
     (source 'git "git@bitbucket.org:art-system/sigma-mrua.git")
     (stage "build" (depends '(kernel build)))
     (stage "modules")
     (stage "install" (depends '(firmware unpack))))

(pkg "chicken"
     (source 'git "https://github.com/bazurbat/chicken-scheme.git")
     (stage "host" "build")
     (stage "host" "install")
     (stage "target" "build" (depends '(chicken install host)))
     (stage "target" "install" (depends '(firmware unpack))))

(pkg "chicken-eggs"
     (source 'git "https://github.com/bazurbat/chicken-eggs.git")
     (stage "host" "install" (depends '(chicken install host)))
     (stage "target" "install"
            (depends '(chicken install target))
            (after '(chicken-eggs install host)
                   '(dbus install))))

(define-firmware-package
  "dbus"
  (source 'dist "dbus-1.6.18.tar.gz")
  (stage "build" (depends '(expat install))))

(define-firmware-package
  "expat"
  (source 'dist "expat-2.1.0.tar.gz"))

(define-firmware-package
  "freetype"
  (source 'dist "freetype-2.5.0.1.tar.bz2")
  (patch "freetype-2.3.2-enable-valid" 1)
  (patch "freetype-2.4.11-sizeof-types" 1)
  (patch "freetype-2.4.12-clean-include" 1))

(define-firmware-package
  "libuv"
  (source 'dist "libuv-v0.10.25.tar.gz"))

(define-firmware-package
  "rsync"
  (source 'dist "rsync-3.1.1.tar.gz"))

(define-firmware-package
  "sqlite"
  (source 'dist "sqlite-autoconf-3080403.tar.gz")
  (patch "sqlite-3.8.1-autoconf-dlopen_check" 1))

(define-firmware-package
  "wpa_supplicant"
  (source 'dist "wpa_supplicant-2.2.tar.gz")
  (patch "wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path" 1)
  (stage "build" (depends '(dbus install))))

(define-firmware-package
  "zlib"
  (source 'dist "zlib-1.2.8.tar.gz"))

(define-firmware-package
  "xtables"
  (source 'dist "iptables-1.4.21.tar.bz2"))

(define-firmware-package
  "xtables-addons"
  (source 'dist "xtables-addons-1.47.1.tar.xz")
  (stage "build" (depends '(xtables install))))

(pkg "ffmpeg"
     (source 'dist "ffmpeg-2.2.1.tar.bz2")
     (stage "host" "build" (depends '(ast-files unpack)))
     (stage "host" "install")
     (stage "target" "build" (depends '(ast-files unpack)))
     (stage "target" "install" (depends '(firmware unpack))))

(pkg "soundtouch"
     (source 'dist "soundtouch-1.8.0.tar.gz")
     (stage "build")
     (stage "install" (depends '(firmware unpack))))

(pkg "astindex"
     (source 'hg "ssh://hg@bitbucket.org/art-system/astindex")
     (stage "unpack" (depends '(karaoke-player unpack))))

(pkg "karaoke-player"
     (source 'hg "ssh://hg@bitbucket.org/art-system/karaoke-player")
     (stage "host" "build" (depends '(astindex unpack)
                                    '(ffmpeg build host)
                                    '(chicken-eggs install host)))
     (stage "host" "install")
     (stage "target" "prepare")
     (stage "target" "build" (depends '(astindex unpack)
                                      '(chicken install target)
                                      '(chicken-eggs install host)
                                      '(dbus install)
                                      '(ffmpeg install target)
                                      '(freetype install)
                                      '(libuv install)
                                      '(mrua build)
                                      '(soundtouch install))
            (if (regexp-search "experimental_network" *flags*)
              (depends '(connman install))))
     (stage "install" (after '(chicken-eggs install target))))

(pkg "firmware"
     (stage "material" (depends '(mrua build)))
     (stage "install" (depends '(ezboot install)
                               '(mrua install)
                               '(kernel image)
                               '(karaoke-player install target)
                               '(dbus install)
                               '(expat install)
                               '(freetype install)
                               '(libuv install)
                               '(rsync install)
                               '(sqlite install)
                               '(wpa_supplicant install)
                               '(zlib install))
            (if (regexp-search "experimental_network" *flags*)
              (depends '(libffi install)
                       '(glib install)
                       '(connman install))))
     (stage "strip"))

(when (regexp-search "jemalloc" *flags*)
  (pkg "jemalloc"
       (source 'dist "jemalloc-3.6.0.tar.bz2")
       (stage "build")
       (stage "install" (depends '(firmware unpack)))))

(when (regexp-search "experimental_network" *flags*)
  (pkg "libffi"
       (source 'dist "libffi-3.1.tar.gz")
       (patch "libffi-3.1-execstack" 1)
       (patch "libffi-3.1-typing_error" 1)
       (stage "build" (depends '(libtool install)))
       (stage "install" (depends '(firmware unpack))))

  (pkg "glib"
       (source 'dist "glib-2.40.2.tar.xz")
       (patch "glib-2.40.0-external-gdbus-codegen" 1)
       (stage "patch" (depends '(libtool install)))
       (stage "build" (depends '(zlib install)
                               '(libffi install)))
       (stage "install" (depends '(firmware unpack))))

  (pkg "connman"
       (source 'dist "connman-1.26.tar.xz")
       (stage "build" (depends '(libtool install)
                               '(dbus install)
                               '(glib install)
                               '(xtables-addons install)))
       (stage "install" (depends '(firmware unpack)))))
