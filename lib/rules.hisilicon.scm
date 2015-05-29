(define (package name . args)
  (apply define-package name
         (stage 'update)
         (stage 'clean)
         (stage 'unpack)
         (stage 'patch)
         args))

(define (firmware-package name . args)
  (apply package name
         (stage 'build)
         (stage 'install (depends (target 'firmware 'unpack)))
         args))

(package 'ast-files
  (source 'git "git@bitbucket.org:art-system/files.git"))

(package 'make
  (source 'dist "make-3.81.tar.bz2")
  (stage 'host 'build)
  (stage 'host 'install))

(package 'android-cmake
  (source 'git "https://github.com/taka-no-me/android-cmake.git"))

(package 'libuv
  (source 'dist "libuv-1.4.2.tar.gz")
  (stage 'target 'build)
  (stage 'target 'install (depends (target 'firmware 'unpack))))

(package 'ffmpeg
  (source 'dist "ffmpeg-2.2.1.tar.bz2")
  (stage 'target 'build   (depends (target 'ast-files 'unpack)))
  (stage 'target 'install (depends (target 'firmware  'unpack))))

(package 'astindex
  (source 'hg "ssh://hg@bitbucket.org/art-system/astindex"
          (directory "karaoke-player/source/astindex"))
  (stage 'unpack (depends (target 'karaoke-player 'unpack))))

(package 'karaoke-player
  (source 'hg "ssh://hg@bitbucket.org/art-system/karaoke-player")
  (stage 'target 'build (depends (target 'android-cmake 'unpack)
                                 (target 'astindex      'unpack)
                                 (target 'ffmpeg        'install 'target)
                                 (target 'libuv         'install 'target)))
  (stage 'target 'install))

(package 'firmware
  (stage 'install (depends (target 'karaoke-player  'install 'target)))
  (stage 'strip)
  (stage 'deploy))

; vim: lw+=package,rootfs-package,kernel-package,firmware-package
