(define (package name . args)
  (apply define-package name
         (stage 'update)
         (stage 'clean)
         (stage 'unpack)
         (stage 'patch)
         args))

(package 'make
  (source 'dist "make-3.82.tar.bz2")
  (stage 'host 'build)
  (stage 'host 'install))
