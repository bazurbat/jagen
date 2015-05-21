(define (package name . args)
  (apply define-package name
         (stage 'update)
         (stage 'clean)
         (stage 'unpack)
         (stage 'patch)
         args))

(package 'ast-files
  (source 'git "git@bitbucket.org:art-system/files.git"))

(package 'make
  (source 'dist "make-3.81.tar.bz2")
  (stage 'host 'build)
  (stage 'host 'install))

(package 'ffmpeg
  (source 'dist "ffmpeg-2.2.1.tar.bz2")
  (stage 'target 'build   (depends (target 'ast-files 'unpack)))
  (stage 'target 'install (depends (target 'firmware  'unpack))))

(package 'firmware
  (stage 'install (depends (target 'ffmpeg 'install 'target)))
  (stage 'strip))

; vim: lw+=package,rootfs-package,kernel-package,firmware-package
