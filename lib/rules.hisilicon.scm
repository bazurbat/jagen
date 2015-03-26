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

(package 'make
  (source 'dist "make-3.81.tar.bz2")
  (stage 'host 'build)
  (stage 'host 'install))

