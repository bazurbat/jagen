(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme load)
        (scheme process-context)
        (srfi 2)
        (srfi 26)
        (chibi regexp)
        (chibi match)
        (chibi show))

(define-record-type
  target
  (make-target name stage config) target?
  (name   target-name)
  (stage  target-stage)
  (config target-config))

(define main
  (match-lambda
    ((_) (show #t "pbuild" nl))
    ((_ "generate" out in)
     (generate-build out in)))
  0)

(define (import-shell-variable p)
  (let* ((k (car p))
         (v (cdr p))
         (rx '(: "ja_" (-> name (+ any))))
         (m (regexp-matches rx k)))
    (if m
      (let* ((submatch (regexp-match-submatch m 'name))
             (name (regexp-replace-all "_" submatch "-")))
        (cons (cons (string->symbol name) v) *env*))
      '())))

(define *env*
  (apply append (map import-shell-variable (get-environment-variables))))

(define (env k)
  (and-let* ((p (assq k *env*))
             (value (cdr p))
             ((not (zero? (string-length value)))))
    value))

(define (generate-build out-file in-file)
  (if (file-exists? out-file) (delete-file out-file))
  (with-output-to-file out-file (cut load in-file)))

(define (pkg name . rest)
  (let loop ((rest rest) (config #f) (prev '()))
    (unless (null? rest)
      (match (car rest)
             (('config config rest ...)
              (loop rest config prev))
             ((stage deps ...)
              (%target (make-target name stage config)
                       (append (list prev) deps))
              (set! prev (list name stage config))))
      (loop (cdr rest) config prev)))
  (show #t nl))

(define (%include file)
  (show #t "include " file nl nl))

(define (%target t deps)
  (match-let ((($ target p n c) t))
             (show #t "build $builddir/" (%name t) ": script"
                   (apply show #f (map (cut %dep <>) deps)) nl
                   (space-to 4)
                   "script = pkg.sh " p " " n " " (if c c "") nl)))

(define (%dep expr)
  (match expr
         (() "")
         (('after targets ...)
          (show #f " ||"
                (apply show #f (map %dep targets))))
         ((n t c ...)
          (let ((c (and (pair? c) (car c))))
            (show #f " $builddir/" (%name (make-target n t c)))))))

(define (%name t)
  (match-let ((($ target p n c) t))
             (show #f p "-" n (if c (show #f "-" c) ""))))
