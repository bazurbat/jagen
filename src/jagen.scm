(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme load)
        (scheme process-context)
        (srfi 26)
        (chibi regexp)
        (chibi match)
        (chibi show))

(define *env* '())

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

(define (import-shell-variable p)
  (let* ((k (car p))
         (v (cdr p))
         (rx '(: "ja_" (-> name (+ any))))
         (m (regexp-matches rx k)))
    (when m
      (let ((n (regexp-match-submatch m 'name)))
        (set! *env* (cons (cons (string->symbol n) v) *env*))))))

(for-each import-shell-variable (get-environment-variables))

(define (generate-build out-file in-file)
  (if (file-exists? out-file) (delete-file out-file))
  (with-output-to-file out-file (lambda ()
                                  (include "src/rules.scm"))))

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
