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
  rule
  (make-rule name variables) rule?
  (name      rule-name)
  (variables rule-variables))

(define-record-type
  build
  (make-build rule outputs inputs variables) build?
  (rule      build-rule)
  (outputs   build-outputs)
  (inputs    build-inputs)
  (variables build-variables))

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

(define (intersperse ls x)
  (if (or (null? ls) (null? (cdr ls)))
    ls
    (let loop ((ls (cdr ls)) (res (list (car ls))))
      (let ((res (cons (car ls) (cons x res))))
        (if (null? (cdr ls))
          (reverse res)
          (loop (cdr ls) res))))))

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
                       (if (null? prev)
                         deps
                         (cons prev deps)))
              (set! prev (list name stage config))))
      (loop (cdr rest) config prev)))
  (show #t nl))

(define (%include file)
  (show #t "include " file nl nl))

(define (%build b)
  (define (target name)
    (show #f "$builddir/" name))

  (define (variable pr)
    (show #f (car pr) " = " (cdr pr)))

  (show #t "build")

  (let loop ((outs (build-outputs b)))
    (unless (null? outs)
      (show #t " " (target (car outs)))
      (loop (cdr outs))))

  (show #t ": " (build-rule b))
  (unless (null? (build-inputs b))
    (show #t " $" nl))

  (let loop ((ins (build-inputs b)))
    (if (null? ins) (show #t nl)
      (match ins
             (('implicit deps ...)
              (show #t (space-to 14) "| $" nl)
              (loop deps))
             (('order-only ins ...)
              (show #t (space-to 13) "|| $" nl)
              (loop ins))
             (other
               (show #t (space-to 16) (target (car other)))
               (unless (null? (cdr ins))
                 (show #t " $" nl))
               (loop (cdr ins))))))

  (let loop ((vars (build-variables b)))
    (unless (null? vars)
      (show #t (space-to 4) (variable (car vars)) nl)
      (loop (cdr vars)))))

(define (%target t deps)
  (define (format-target t . sep)
    (let ((sep (or (and (null? sep) "-") (car sep))))
      (match-let ((($ target n s c) t))
                 (if c
                   (show #f n sep s sep c)
                   (show #f n sep s)))))

  (define format-dep
    (match-lambda
      ('after
       'order-only)
      ((n s c ...)
       (format-target (make-target n s (if (null? c) #f (car c)))))))

  (%build
    (make-build "script"
                (list (format-target t))
                (map format-dep deps)
                `(("script" . ,(show #f "pkg.sh " (format-target t " ")))))))
