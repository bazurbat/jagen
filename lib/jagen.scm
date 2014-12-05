(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme load)
        (scheme process-context)
        (srfi 1)
        (srfi 2)
        (srfi 26)
        (chibi filesystem)
        (chibi match)
        (chibi pathname)
        (chibi regexp)
        (chibi show))

(define *width* 4)

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

(define-record-type
  source@
  (source type location)
  source?
  (type     source-type)
  (location source-location))

(define-record-type
  patch@
  (patch name strip)
  patch?
  (name  patch-name)
  (strip patch-strip))

(define-record-type
  stage@
  (make-stage name config deps)
  stage?
  (name   stage-name)
  (config stage-config)
  (deps   stage-deps))

(define (stage . args)
  (match args
         (((? string? config) (? string? name) deps ...)
          (make-stage name config deps))
         (((? string? name) deps ...)
          (make-stage name #f deps))
         (((? string? name))
          (make-stage name #f '()))))

(define (depends . deps)
  (if (pair? deps) (cons 'depends (car deps)) deps))
(define (after . deps)
  (if (pair? deps) (cons 'after (car deps)) deps))

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

(define (%sh:variable name value)
  (show #t name "=\"" value "\"" nl))

(define (source->string source)
  (let ((type     (source-type source))
        (location (source-location source)))
    (case type
      ((dist)
       (string-append "$pkg_dist_dir/" location))
      ((git hg)
       (string-append (symbol->string type) " " location))
      (else location))))

(define (pkg:generate name source)
  (define (create-script)
    (show #t "#!/bin/sh" nl)
    (%sh:variable "p_source" (source->string source)))
  (let ((path (make-path (env 'build-include-dir)
                         (string-append name ".sh"))))
    (create-directory* (path-directory path))
    (with-output-to-file path create-script)))

(define (pkg name . args)
  (let ((source (source #f ""))
        (patches '())
        (stages (map (cut make-stage <> #f '())
                     (reverse (list "update" "clean" "unpack" "patch")))))
    (show #t "pkg: " name nl)
    (do ((args args (cdr args))) ((null? args))
      (let ((arg (car args)))
        (cond ((source? arg)
               (set! source arg))
              ((patch? arg)
               (set! patches (cons arg patches)))
              ((stage? arg)
               (set! stages (cons arg stages))))))
    (pkg:generate name source)
    (for-each
      (lambda (s)
        (%target (make-target name (stage-name s) (stage-config s)) '()))
      (reverse stages))
    (show #t "stages: " stages nl)))

(define (%pkg name stages)
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
