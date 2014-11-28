(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme load)
        (scheme process-context)
        (srfi 1)
        (srfi 2)
        (srfi 26)
        (chibi regexp)
        (chibi match)
        (chibi show))

(define-record-type
  rule
  (make-rule name variables)
  rule?
  (name      rule-name)
  (variables rule-variables))

(define-record-type
  build
  (make-build rule outputs inputs variables)
  build?
  (rule      build-rule)
  (outputs   build-outputs)
  (inputs    build-inputs)
  (variables build-variables))

(define-record-type
  target
  (make-target name stage config)
  target?
  (name   target-name)
  (stage  target-stage)
  (config target-config))

(define-record-type
  link
  (make-link type from to)
  link?
  (type link-type)
  (from link-from)
  (to   link-to))

(define-record-type
  package
  (make-package name)
  package?
  (name   package-name))

(define *packages* '())
(define *stages* '())
(define *width* 4)

(define main
  (match-lambda
    ((_) (show #t "pbuild" nl))
    ((_ "generate" out in)
     (generate-build out in)))
  0)

(define (import-shell-variable p)
  (let* ((k (car p))
         (v (cdr p))
         (rx '(: "pkg_" (-> name (+ any))))
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

#;(define (pkg name . stages)
  (define (find-stage stage lst)
    (find (lambda (s) (eq? (car s) (car stage))) lst))

  (define (pull-stage stage lst)
    (let ((found (find-stage stage lst)))
      (if found found stage)))

  (define match-config
    (match-lambda
      (('config config stages ...)
       (map (cut match-stage <> config) stages))
      ((stage ...)
       (list (match-stage stage #f)))))

  (define (match-stage stage config)
    (match stage
           ((stage)
            (list (list (make-link 'implicit (make-target name stage #f) '()))))
           ((stage dependencies ...)
            (map (cut match-inputs (make-target name stage config) <>)
                 dependencies))))

  (define (match-inputs target dep)
    (match dep
           (('after deps ...)
            (map (cut match-link 'order-only target <>) deps))
           ((dep ...)
            (list (match-link 'explicit target dep)))))

  (define (match-link type from to)
    (match to
           ((package stage)
            (make-link type from (make-target package stage #f)))
           ((package stage config)
            (make-link type from (make-target package stage config)))))

  (define (collect-stages stages)
    (define pre '((update) (clean) (unpack) (patch)))

    (let loop ((stages (append (map (cut pull-stage <> stages) pre)
                               (remove (cut find-stage <> pre) stages))))
      (if (null? stages)
        '()
        (cons (match-config (car stages)) (loop (cdr stages))))))

  (set! *packages* (cons (make-package name) *packages*))
  (set! *stages* (append *stages* (apply append (apply append (apply append (collect-stages stages)))))))

(define (pkg name . stages)
  (define (find-stage stage lst)
    (find (lambda (s) (eq? (car s) (car stage))) lst))
  (define (pull-stage stage lst)
    (let ((found (find-stage stage lst)))
      (if found found stage)))
  (define pre '((update) (clean) (unpack) (patch)))
  (let loop ((stages (append (map (cut pull-stage <> stages) pre)
                             (remove (cut find-stage <> pre) stages)))
             (config #f) (prev '()))
    (unless (null? stages)
      (match (car stages)
             (('config config stages ...)
              (loop stages config prev))
             ((stage deps ...)
              (%target (make-target name stage config)
                       (if (null? prev)
                         deps
                         (cons prev deps)))
              (set! prev (list name stage config))))
      (loop (cdr stages) config prev)))
  (show #t nl))

(define (%include file)
  (show #t "include " file nl nl))

(define (%variable name value . level)
  (let ((level (or (and (pair? level) (car level)) 0)))
    (show #t (space-to (* level *width*)) name " = " value nl)))

(define (%rule r)
  (define (variable p)
    (%variable (car p) (cdr p) 1))

  (show #t "rule " (rule-name r) nl)
  (for-each variable (rule-variables r))
  (show #t nl))

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
                `(("script" . ,(show #f "jagen-pkg " (format-target t " ")))))))
