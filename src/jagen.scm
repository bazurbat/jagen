(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme load)
        (srfi 26)
        (chibi match)
        (chibi show))

(define-record-type
  target
  (make-target pkg name config) target?
  (pkg    target-pkg)
  (name   target-stage)
  (config target-config))

(define main
  (match-lambda
    ((_) (show #t "pbuild" nl))
    ((_ "generate" out in)
     (generate-build out in)))
  0)

(define-syntax build
  (syntax-rules ()
    ((build pkg rule rules ...)
     (show #t (apply show #f (map (cut %build 'pkg #f <>)
                                  (list 'rule 'rules ...)))
           nl))))

(define (generate-build out-file in-file)
  (if (file-exists? out-file) (delete-file out-file))
  (with-output-to-file out-file (lambda ()
                                  (include "env.scm")
                                  (include "build.scm"))))

(define (%include file)
  (show #t "include " file nl nl))

(define (%build pkg config expr)
  (match expr
         (('config name targets ...)
          (apply show #f (map (cut %build pkg name <>) targets)))
         ((name deps ...)
          (%target (make-target pkg name config) deps))))

(define (%target t deps)
  (match-let ((($ target p n c) t))
             (show #f "build $builddir/" (%name t) ": script"
                   (apply show #f (map (cut %dep <>) deps)) nl
                   (space-to 4)
                   "script = pkg.sh " p " " n " " (if c c "") nl)))

(define (%dep expr)
  (match expr
         (('after targets ...)
          (show #f " ||"
                (apply show #f (map %dep targets))))
         ((n t c ...)
          (let ((c (and (pair? c) (car c))))
            (show #f " $builddir/" (%name (make-target n t c)))))))

(define (%name t)
  (match-let ((($ target p n c) t))
             (show #f p "-" n (if c (show #f "-" c) ""))))
