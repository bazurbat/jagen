;{{{ imports
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

;}}}
;{{{ helper functions

(define (compose f g)
  (lambda args (f (apply g args))))

(define (mapreduce f g id xs)
  (cond ((null? xs) id)
        (else (g (f (car xs)) (mapreduce f g id (cdr xs))))))

(define (run-with-state state . args)
  (define (bind procs)
    (lambda (state)
      (let loop ((procs procs) (state state))
        (if (null? procs)
          state
          (loop (cdr procs) ((car procs) state))))))
  ((bind args) state))

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

(define (env k)
  (and-let* ((p (assq k *env*))
             (value (cdr p))
             ((not (zero? (string-length value)))))
    value))

;}}}
;{{{ globals

(define *packages* '())
(define *width* 4)

(define *env*
  (apply append (map import-shell-variable (get-environment-variables))))

(define *flags* (or (env 'flags) ""))

;}}}
;{{{ types

(define-record-type <state>
  (make-state depth) state?
  (depth state-depth))

(define-record-type <variable>
  (make-variable name value) variable?
  (name  variable-name)
  (value variable-value))

(define-record-type <rule>
  (make-rule name variables) rule?
  (name      rule-name)
  (variables rule-variables))

(define-record-type <build>
  (make-build rule outputs depends variables) build?
  (rule      build-rule)
  (outputs   build-outputs)
  (depends   build-depends)
  (variables build-variables))

(define-record-type <target>
  (make-target name stage config) target?
  (name   target-name)
  (stage  target-stage)
  (config target-config))

(define-record-type <stage>
  (make-stage name config depends) stage?
  (name    stage-name)
  (config  stage-config)
  (depends stage-depends set-stage-depends!))

(define-record-type <dependency>
  (make-dependency type target) dependency?
  (type   dependency-type)
  (target dependency-target))

(define-record-type <source>
  (make-source type location) source?
  (type     source-type)
  (location source-location))

(define-record-type <patch>
  (make-patch name strip) patch?
  (name  patch-name)
  (strip patch-strip))

(define-record-type <package>
  (make-package name source patches stages) package?
  (name    package-name)
  (source  package-source)
  (patches package-patches)
  (stages  package-stages))

;}}}
;{{{ constructors

(define target
  (match-lambda*
    ((name stage config)
     (make-target name stage config))
    ((name stage)
     (make-target name stage #f))))

(define (source type location)
  (lambda (pkg)
    (make-package
      (package-name pkg)
      (make-source type location)
      (package-patches pkg)
      (package-stages pkg))))

(define (patch name strip)
  (lambda (pkg)
    (make-package
      (package-name pkg)
      (package-source pkg)
      (append (package-patches pkg) (list (make-patch name strip)))
      (package-stages pkg))))

(define (stage . args)
  (define (create-stage args)
    (match args
      (((? symbol? config) (? symbol? name) deps ...)
       (apply run-with-state (make-stage name config '()) deps))
      (((? symbol? name) deps ...)
       (apply run-with-state (make-stage name #f '()) deps))
      (((? symbol? name))
       (make-stage name #f '()))))

  (define (previous? this next)
    (let ((this-config (stage-config this))
          (next-config (stage-config next)))
      (or (eq? this-config next-config) (not next-config))))

  (define (same? this next)
    (let ((this-name (stage-name this))
          (next-name (stage-name next))
          (this-config (stage-config this))
          (next-config (stage-config next)))
      (and (eq? this-name next-name) (eq? this-config next-config))))

  (define (find-previous this stages)
    (find (cut previous? this <>) stages))

  (define (find-same this stages)
    (find (cut same? this <>) stages))

  (lambda (pkg)
    (let* ((stage (create-stage args))
           (stages (package-stages pkg))
           (same (find-same stage stages)))
      (define (add-previous stage)
        (let ((previous (find-previous stage stages)))
          (if previous
            (make-stage
              (stage-name stage)
              (stage-config stage)
              (cons (make-dependency 'explicit
                                     (target (package-name pkg)
                                             (stage-name previous)
                                             (stage-config previous)))
                    (stage-depends stage)))
            stage)))
      (make-package
        (package-name pkg)
        (package-source pkg)
        (package-patches pkg)
        (cond (same
                (set-stage-depends! same (append (stage-depends same)
                                                 (stage-depends stage)))
                stages)
              (else (cons (add-previous stage) stages)))))))

(define (depends . args)
  (lambda (stage)
    (make-stage
      (stage-name stage)
      (stage-config stage)
      (append (stage-depends stage)
              (map (cut make-dependency 'explicit <>) (remove not args))))))

(define (after . args)
  (lambda (stage)
    (make-stage
      (stage-name stage)
      (stage-config stage)
      (append (stage-depends stage)
              (map (cut make-dependency 'order-only <>) (remove not args))))))

;}}}
;{{{ common generators

(define (%target t)
  (lambda (state)
    (let ((n (target-name   t))
          (s (target-stage  t))
          (c (target-config t)))
      (if c
        (show #f n "-" s "-" c)
        (show #f n "-" s)))))

(define (%variable name value . level)
  (let ((level (or (and (pair? level) (car level)) 0)))
    (show #t (space-to (* level *width*)) name " = " value nl)))

;}}}
;{{{ ninja generators

(define (%ninja:variable v)
  (lambda (state)
    (show #f nl (space-to (* (state-depth state) *width*))
          (variable-name v) " = " (variable-value v))))

(define (%ninja:build b)
  (define (%targets state ts)
    (mapreduce %target (lambda (p r) (show #f "$builddir/" (p state))) "" ts))
  (define (%dependency state d)
    (let ((t (dependency-target d)))
      (show #f (space-to 16) "$builddir/" ((%target t) state))))
  (define (filter-type type ds)
    (filter (lambda (d) (eq? type (dependency-type d))) ds))

  (let* ((rule       (build-rule      b))
         (out        (build-outputs   b))
         (depends    (build-depends   b))
         (explicit   (filter-type 'explicit depends))
         (order-only (filter-type 'order-only depends))
         (vars       (build-variables b)))
    (lambda (state)
      (show #f "build " (%targets state out) ": " rule
            (joined/prefix (cut %dependency state <>) explicit
                           (show #f " $" nl))
            (if (pair? order-only)
              (show #f " $" nl (space-to 13) "|| $" nl
                    (joined (cut %dependency state <>) order-only
                            (show #f " $" nl)))
              "")
            (mapreduce %ninja:variable
                       (lambda (p r) (show #f (p (make-state 1)))) ""
                       (build-variables b))
            nl))))

(define (%ninja:rule r)
  (define (variable p)
    (%variable (car p) (cdr p) 1))
  (show #t "rule " (rule-name r) nl)
  (for-each variable (rule-variables r))
  (show #t nl))

(define (stage->build name stage)
  (let ((sn (stage-name stage))
        (sc (stage-config stage)))
    (make-build
      "script"
      (list (make-target name sn sc))
      (stage-depends stage)
      (list (make-variable "script"
                           (let ((args (if sc (list name sn sc) (list name sn))))
                             (show #f "jagen-pkg "
                                   (joined (lambda (x) x) args " "))))))))

(define (%ninja:package pkg)
  (let* ((state (make-state 0))
         (name (package-name pkg))
         (builds (map (cut stage->build name <>) (reverse (package-stages pkg)))))
    (show #t (joined (lambda (x) x)
                     (map (cut <> state) (map %ninja:build builds)))
          nl)))

(define (%ninja:output packages)
  (%variable "builddir" (env 'build-dir))
  (show #t nl)
  (%ninja:rule (make-rule
                 "command"
                 (list (cons "command" "$command"))))
  (%ninja:rule (make-rule
                 "script"
                 (list (cons "command"
                             (string-append (env 'bin-dir)
                                            "/$script && touch $out")))))
  (for-each %ninja:package packages))

;}}}
;{{{ shell generators

(define (%sh:variable name value)
  (show #f name "=\"" value "\"" nl))

(define (%sh:source source)
  (let ((type     (source-type source))
        (location (source-location source)))
    (case type
      ((dist)
       (show #f "$pkg_dist_dir/" location))
      ((git hg)
       (show #f (symbol->string type) " " location))
      (else location))))

(define (%sh:patch patch)
  (let ((name  (patch-name patch))
        (strip (patch-strip patch)))
    (show #f (space-to 4)
          "p_patch " strip " \"" name "\"")))

(define (generate-include-script pkg)
  (define (create-script)
    (let ((source (package-source pkg))
          (patches (package-patches pkg)))
      (show #t "#!/bin/sh" nl)
      (when source
        (show #t (%sh:variable "p_source" (%sh:source source))))
      (unless (null? patches)
        (show #t nl "pkg_patch_pre() {" nl
              (joined/suffix %sh:patch patches nl)
              "}" nl))))

  (let* ((name (package-name pkg))
         (path (make-path (env 'build-include-dir)
                          (show #f (symbol->string name) ".sh"))))
    (create-directory* (path-directory path))
    (with-output-to-file path create-script)))

;}}}

(define (define-package name . rest)
  (let* ((state (make-package name #f '() '()))
         (pkg (apply run-with-state state rest)))
    (set! *packages* (cons pkg *packages*))
    pkg))

(define (load-packages pathname)
  (load pathname)
  (set! *packages* (reverse *packages*))
  *packages*)

(define (cmd:generate out-file in-file)
  (let ((packages (load-packages in-file)))
    (if (file-exists? out-file) (delete-file out-file))
    (with-output-to-file out-file (cut %ninja:output packages))
    (for-each generate-include-script packages)))

(define main
  (match-lambda
    ((_) (show #t "pbuild" nl))
    ((_ "generate" out in)
     (cmd:generate out in)))
  0)
