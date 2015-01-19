(define (package name . args)
  (apply define-package name
         (stage 'update)
         (stage 'clean)
         (stage 'unpack)
         (stage 'patch)
         args))

(define (rootfs-package name . args)
  (apply package name
         (stage 'build (depends (target 'rootfs 'build)))
         (stage 'install)
         args))

(define (kernel-package name . args)
  (apply package name
         (stage 'build (depends (target 'kernel 'build)))
         (stage 'install)
         args))

(define (firmware-package name . args)
  (apply package name
         (stage 'build)
         (stage 'install (depends (target 'firmware 'unpack)))
         args))

; base

(package 'ast-files
  (source 'git "git@bitbucket.org:art-system/files.git"))

(package 'linux
  (source 'git "git@bitbucket.org:art-system/linux.git"))

(package 'xsdk
  (source 'dist "${cpukeys}.tar.gz"))

(package 'ucode
  (source 'dist "mruafw_SMP8654F_prod_3_9_2.tgz")
  (stage 'install (depends (target 'firmware 'unpack))))

; tools

(package 'make
  (source 'dist "make-3.80.tar.bz2")
  (stage 'host 'build)
  (stage 'host 'install))

; host

(package 'libtool
  (source 'dist "libtool-2.4.3.tar.xz")
  (patch "libtool-2.4.3-no-clean-gnulib" 1)
  (patch "libtool-2.4.3-test-cmdline_wrap" 1)
  (stage 'build)
  (stage 'install))

; utils

(package 'utils
  (source 'git "git@bitbucket.org:art-system/sigma-utils.git")
  (stage 'host   'build)
  (stage 'host   'install)
  (stage 'target 'build (depends (target 'gpgme 'install)
                                 (target 'dbus  'install)))
  (stage 'target 'install))

; boot

(rootfs-package 'ezboot
  (source 'git "git@bitbucket.org:art-system/sigma-ezboot.git"))

; fails to build with new toolchain
;(rootfs-package "yamon")

; debugging

(when (string=? "Debug" (env 'build-type))
  (package 'gdb
    (source 'dist "gdb-7.6.2.tar.bz2")
    (patch "05_all_readline-headers" 1)
    (patch "10_all_gdb-7.6-cpuid" 1)
    (patch "15_all_gdb-7.6-btrace" 1)
    (stage 'host 'build)
    (stage 'host 'install))

  (rootfs-package 'gdbserver
    (source 'dist "gdb-7.6.2.tar.bz2"))

  (rootfs-package 'strace
    (source 'dist "strace-4.8.tar.xz")))

; rootfs

(package 'rootfs
  (source 'git "git@bitbucket.org:art-system/sigma-rootfs.git")
  (stage 'build   (after   (target 'ast-files  'unpack)
                           (target 'xsdk       'unpack)
                           (target 'make       'install 'host)))
  (stage 'install (depends (target 'kernel     'install)
                           (target 'busybox    'install)
                           (target 'gnupg      'install)
                           (target 'loop-aes   'install)
                           (target 'mrua       'modules)
                           (target 'ntpclient  'install)
                           (if (regexp-search "new_kernel" *flags*)
                             #f
                             (target 'ralink 'install))
                           (target 'util-linux 'install)
                           (target 'utils      'install 'target))))

(rootfs-package 'busybox
  (source 'dist "busybox-1.22.1.tar.bz2"))

(rootfs-package 'ntpclient
  (source 'dist "ntpclient-2010.tar.gz"))

(rootfs-package 'util-linux
  (source 'dist "util-linux-2.23.2.tar.xz")
  (patch "util-linux-2.23.2" 1))

; gpgme

(rootfs-package 'libgpg-error
  (source 'dist "libgpg-error-1.17.tar.bz2"))

(rootfs-package 'libassuan
  (source 'dist "libassuan-2.1.2.tar.bz2")
  (stage 'build (depends (target 'libgpg-error 'install))))

(rootfs-package 'gpgme
  (source 'dist "gpgme-1.5.1.tar.bz2")
  (stage 'build (depends (target 'libassuan 'install))))

(rootfs-package 'gnupg
  (source 'dist "gnupg-1.4.18.tar.bz2"))

; kernel

(package 'kernel
  (source 'git "git@bitbucket.org:art-system/sigma-kernel.git")
  (stage 'build (depends (target 'linux  'unpack)
                         (target 'ezboot 'build)
                         (target 'rootfs 'build)))
  (stage 'install)
  (stage 'image (depends (target 'rootfs 'install))))

(unless (regexp-search "new_kernel" *flags*)
  (kernel-package 'ralink
    (source 'dist "DPO_RT5572_LinuxSTA_2.6.1.3_20121022.tar.bz2")
    (patch "DPO_RT5572_LinuxSTA_2.6.1.3_20121022-no-tftpboot" 1)
    (patch "DPO_RT5572_LinuxSTA_2.6.1.3_20121022-encrypt" 1)))

(kernel-package 'loop-aes
  (source 'dist "loop-AES-v3.7a.tar.bz2"))

(package 'mrua
  (source 'git "git@bitbucket.org:art-system/sigma-mrua.git")
  (stage 'build   (depends (target 'kernel 'build)))
  (stage 'modules)
  (stage 'install (depends (target 'firmware 'unpack))))

(package 'chicken
  (source 'git "https://github.com/bazurbat/chicken-scheme.git")
  (stage 'host   'build)
  (stage 'host   'install)
  (stage 'target 'build   (depends (target 'chicken  'install 'host)))
  (stage 'target 'install (depends (target 'firmware 'unpack))))

(package 'chicken-eggs
  (source 'git "https://github.com/bazurbat/chicken-eggs.git")
  (stage 'host   'install (depends (target 'chicken 'install 'host)))
  (stage 'target 'install
         (depends (target 'chicken      'install 'target))
         (depends (target 'sqlite       'install))
         (after   (target 'chicken-eggs 'install 'host)
                  (target 'dbus         'install))))

(firmware-package 'dbus
  (source 'dist "dbus-1.6.18.tar.gz")
  (stage 'build (depends (target 'expat 'install))))

(firmware-package 'expat
  (source 'dist "expat-2.1.0.tar.gz"))

(firmware-package 'freetype
  (source 'dist "freetype-2.5.0.1.tar.bz2")
  (patch "freetype-2.3.2-enable-valid" 1)
  (patch "freetype-2.4.11-sizeof-types" 1)
  (patch "freetype-2.4.12-clean-include" 1))

(firmware-package 'libuv
  (source 'dist "libuv-v0.10.25.tar.gz"))

(firmware-package 'rsync
  (source 'dist "rsync-3.1.1.tar.gz"))

(firmware-package 'sqlite
  (source 'dist "sqlite-autoconf-3080403.tar.gz")
  (patch "sqlite-3.8.1-autoconf-dlopen_check" 0))

(firmware-package 'wpa_supplicant
  (source 'dist "wpa_supplicant-2.2.tar.gz")
  (patch "wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path" 1)
  (stage 'build (depends (target 'dbus 'install))))

(firmware-package 'zlib
  (source 'dist "zlib-1.2.8.tar.gz"))

(firmware-package 'xtables
  (source 'dist "iptables-1.4.21.tar.bz2"))

(firmware-package 'xtables-addons
  (source 'dist "xtables-addons-1.47.1.tar.xz")
  (stage 'build (depends (target 'xtables 'install))))

(package 'ffmpeg
  (source 'dist "ffmpeg-2.2.1.tar.bz2")
  (stage 'host   'build   (depends (target 'ast-files 'unpack)))
  (stage 'host   'install)
  (stage 'target 'build   (depends (target 'ast-files 'unpack)))
  (stage 'target 'install (depends (target 'firmware  'unpack))))

(package 'soundtouch
  (source 'dist "soundtouch-1.8.0.tar.gz")
  (stage 'build)
  (stage 'install (depends (target 'firmware 'unpack))))

(package 'astindex
  (source 'hg "ssh://hg@bitbucket.org/art-system/astindex")
  (stage 'unpack (depends (target 'karaoke-player 'unpack))))

(package 'karaoke-player
  (source 'hg "ssh://hg@bitbucket.org/art-system/karaoke-player")
  (stage 'host   'build (depends (target 'astindex     'unpack)
                                 (target 'ffmpeg       'build   'host)
                                 (target 'chicken-eggs 'install 'host)))
  (stage 'host   'install)
  (stage 'target 'prepare)
  (stage 'target 'build (depends (target 'astindex     'unpack)
                                 (target 'chicken      'install 'target)
                                 (target 'chicken-eggs 'install 'host)
                                 (target 'dbus         'install)
                                 (target 'ffmpeg       'install 'target)
                                 (target 'freetype     'install)
                                 (target 'libuv        'install)
                                 (target 'mrua         'build)
                                 (target 'soundtouch   'install)
                                 (target 'connman      'install)))
  (stage 'target 'install (after (target 'chicken-eggs 'install 'target))))

(package 'firmware
  (stage 'material (depends (target 'mrua           'build)))
  (stage 'install  (depends (target 'ezboot         'install)
                            (target 'mrua           'install)
                            (target 'kernel         'image)
                            (target 'karaoke-player 'install 'target)
                            (target 'dbus           'install)
                            (target 'expat          'install)
                            (target 'freetype       'install)
                            (target 'libuv          'install)
                            (target 'rsync          'install)
                            (target 'sqlite         'install)
                            (target 'wpa_supplicant 'install)
                            (target 'zlib           'install)
                            (target 'libffi         'install)
                            (target 'glib           'install)
                            (target 'connman        'install)))
  (stage 'strip))

(when (regexp-search "jemalloc" *flags*)
  (package 'jemalloc
    (source 'dist "jemalloc-3.6.0.tar.bz2")
    (stage 'build)
    (stage 'install (depends (target 'firmware 'unpack)))))

(package 'libffi
  (source 'dist "libffi-3.1.tar.gz")
  (patch "libffi-3.1-execstack" 0)
  (patch "libffi-3.1-typing_error" 0)
  (stage 'build   (depends (target 'libtool  'install)))
  (stage 'install (depends (target 'firmware 'unpack))))

(package 'glib
  (source 'dist "glib-2.40.2.tar.xz")
  (patch "glib-2.40.0-external-gdbus-codegen" 1)
  (stage 'patch   (depends (target 'libtool  'install)))
  (stage 'build   (depends (target 'zlib     'install)
                           (target 'libffi   'install)))
  (stage 'install (depends (target 'firmware 'unpack))))

(package 'connman
  (source 'dist "connman-1.26.tar.xz")
  (stage 'build   (depends (target 'libtool        'install)
                           (target 'dbus           'install)
                           (target 'glib           'install)
                           (target 'xtables-addons 'install)))
  (stage 'install (depends (target 'firmware       'unpack))))

; vim: lw+=package,rootfs-package,kernel-package,firmware-package
