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

(define target
  (match-lambda*
    ((name stage config)
     (make-target name stage config))
    ((name stage)
     (make-target name stage #f))))

(define-record-type <source>
  (make-source type location) source?
  (type     source-type)
  (location source-location))

(define (source type location)
  (lambda (pkg)
    (make-package
      (package-name pkg)
      (make-source type location)
      (package-patches pkg)
      (package-stages pkg))))

(define-record-type <patch>
  (make-patch name strip) patch?
  (name  patch-name)
  (strip patch-strip))

(define (patch name strip)
  (lambda (pkg)
    (make-package
      (package-name pkg)
      (package-source pkg)
      (make-patch name strip)
      (package-stages pkg))))

(define-record-type <stage>
  (make-stage name config depends) stage?
  (name    stage-name)
  (config  stage-config)
  (depends stage-depends set-stage-depends!))

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
              (cons (target (package-name pkg)
                            (stage-name previous)
                            (stage-config previous))
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
      (append (stage-depends stage) args))))

(define (after . args)
  (lambda (stage)
    (make-stage
      (stage-name stage)
      (stage-config stage)
      (append (stage-depends stage) args))))

(define-record-type <package>
  (make-package name source patches stages) package?
  (name    package-name)
  (source  package-source)
  (patches package-patches)
  (stages  package-stages))

(define (%target t)
  (lambda (state)
    (let ((n (target-name   t))
          (s (target-stage  t))
          (c (target-config t)))
      (if c
        (show #f n "-" s "-" c)
        (show #f n "-" s)))))

(define (%ninja:variable v)
  (lambda (state)
    (show #f (space-to (* (state-depth state) *width*))
          (variable-name v) " = " (variable-value v))))

(define (%ninja:build b)
  (define (targets state ts)
    (mapreduce %target (lambda (p r) (show #f "$builddir/" (p state))) "" ts))
  (define (deps state ds)
    (map (compose (cut show #f (space-to 16) "$builddir/" <>)
                  (cut <> state))
         (map %target ds)))

  (let ((rule    (build-rule      b))
        (out     (build-outputs   b))
        (depends (build-depends   b))
        (vars    (build-variables b)))
    (lambda (state)
      (show #f "build " (targets state out) ": " rule
            (joined/prefix (lambda (x) x) (deps state depends)
                           (show #f " $" nl)) nl
            (mapreduce %ninja:variable
                       (lambda (p r) (show #f (p (make-state 1)))) ""
                       (build-variables b))
            nl))))

(define (define-package name . rest)
  (define (create-package name)
    (apply run-with-state (make-package name #f '() '()) rest))

  (define (stage->build stage)
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

  (let* ((state (make-state 0))
         (pkg (create-package name))
         (builds (map (cut stage->build <>) (reverse (package-stages pkg)))))
    (show #t (joined (lambda (x) x)
                     (map (cut <> state) (map %ninja:build builds)))
          nl)))

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
