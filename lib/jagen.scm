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
        (chibi process)
        (chibi regexp)
        (chibi show)
        (chibi string)
        (chibi))

;}}}
;{{{ helper functions

(define (compose f g)
  (lambda args (f (apply g args))))

(define (conjoin . preds)
  (lambda (x)
    (let loop ((preds preds))
      (or (null? preds)
          (and ((car preds) x)
               (loop (cdr preds)))))))

(define (mapreduce f g id xs)
  (cond ((null? xs) id)
        (else (g (f (car xs)) (mapreduce f g id (cdr xs))))))

(define (call-with-state state . procs)
  (define (bind procs)
    (lambda (state)
      (let loop ((procs procs) (state state))
        (if (null? procs)
          state
          (loop (cdr procs) ((car procs) state))))))
  ((bind procs) state))

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
  (make-source type location directory branch) source?
  (type      source-type)
  (location  source-location)
  (directory source-directory)
  (branch    source-branch))

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

(define-syntax source
  (er-macro-transformer
    (lambda (expr rename compare)
      (let ((type     (list-ref  expr 1))
            (location (list-ref  expr 2))
            (rest     (list-tail expr 3)))
        `(,(rename 'let) ((directory ,(rename 'source:directory))
                          (branch    ,(rename 'source:branch)))
           (,(rename 'source:) ,type ,location ,@rest))))))

(define (source: type location . rest)
  (lambda (pkg)
    (make-package
      (package-name pkg)
      (apply call-with-state (make-source type location #f #f) rest)
      (package-patches pkg)
      (package-stages pkg))))

(define (source:directory pathname)
  (lambda (s)
    (make-source
      (source-type s)
      (source-location s)
      pathname
      (source-branch s))))

(define (source:branch name)
  (lambda (s)
    (make-source
      (source-type s)
      (source-location s)
      (source-directory s)
      name)))

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
       (apply call-with-state (make-stage name config '()) deps))
      (((? symbol? name) deps ...)
       (apply call-with-state (make-stage name #f '()) deps))
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

(define (target->string target)
  (let ((n (target-name   target))
        (s (target-stage  target))
        (c (target-config target)))
    (if (and c (not s))
      (error "target stage is not set" n c)
      (show #f (joined each (remove not (list n s c)) "-")))))

(define (%variable name value . level)
  (let ((level (or (and (pair? level) (car level)) 0)))
    (show #t (space-to (* level *width*)) name " = " value nl)))

;}}}
;{{{ ninja generators

(define %ninja:newline
  (show #f " $" nl))

(define (%ninja:variable v)
  (lambda (state)
    (show #f nl (space-to (* (state-depth state) *width*))
          (variable-name v) " = " (variable-value v))))

(define (%ninja:build b)
  (define (%dependency d)
    (show #f (space-to 16) (target->string (dependency-target d))))
  (define (filter-type type ds)
    (filter (lambda (d) (eq? type (dependency-type d))) ds))

  (let* ((rule       (build-rule      b))
         (out        (build-outputs   b))
         (depends    (build-depends   b))
         (explicit   (filter-type 'explicit depends))
         (order-only (filter-type 'order-only depends))
         (vars       (build-variables b)))
    (lambda (state)
      (show #f "build " (joined target->string out " ") ": " rule
            (joined/prefix %dependency explicit %ninja:newline)
            (if (pair? order-only)
              (show #f " $" nl (space-to 13) "|| $" nl
                    (joined %dependency order-only %ninja:newline))
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
         (builds (map (cut stage->build name <>) (package-stages pkg))))
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

(define (shell:variable name value)
  (if value (show #f name "=\"" value "\"" nl) ""))

(define (shell:package-source pkg)
  (let* ((source   (package-source  pkg))
         (type     (source-type     source))
         (location (source-location source)))
    (define source
      (case type
        ((git hg) (show #f (symbol->string type) " " location))
        ((dist)   (show #f "$pkg_dist_dir/"          location))
        (else location)))
    (show #f (shell:variable "p_source" source))))

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
      (show #t (shell:package-source pkg))
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
;{{{ source handling

(define (src:ensure-exists path)
  (unless (file-exists? path)
    (die "The source directory is not exists:" path)))

(define (src:type path)
  (cond ((file-exists? (make-path path ".git")) 'git)
        ((file-exists? (make-path path ".hg")) 'hg)
        (else #f)))

(define (src:head path)
  (src:ensure-exists path)
  (let* ((cmd (case (src:type path)
                ((git) "git rev-parse HEAD")
                ((hg)  "hg id -i")))
         (res (with-directory path (cut process->string cmd))))
    (string-trim-right res #\newline)))

(define (src:dirty? path)
  (src:ensure-exists path)
  (let* ((cmd (case (src:type path)
                ((git) "git status --porcelain")
                ((hg)  "hg status")))
         (res (with-directory path (cut process->string cmd))))
    (not (string-null? res))))

(define (src:branch-exists? path branch)
  (src:ensure-exists path)
  (let* ((cmd (case (src:type path)
                ((git) `("git" "branch" "--list" ,branch))
                ((hg) (die "Hg branches support is not implemented"))))
         (res (with-directory path (cut process->string cmd))))
    (not (string-null? res))))

(define (src:clone type src dst)
  (print:debug "src" "clone" type src dst)
  (let ((cmd (case type
               ((git)
                `("git" "clone" "--progress" "--depth" "1"
                  "--no-single-branch" "--no-checkout" ,src ,dst))
               ((hg)
                `("hg" "clone" "-r" "tip" ,src ,dst)))))
    (system:command cmd)))

(define (src:fetch path)
  (print:debug "src" "fetch" path)
  (let ((cmd (case (src:type path)
               ((git) "git fetch --progress -np")
               ((hg)  "hg pull"))))
    (system:command cmd (in-directory path))))

(define (src:pull path)
  (print:debug "src" "pull" path)
  (let ((cmd (case (src:type path)
               ((git) "git pull --progress --ff-only")
               ((hg)  "hg pull -u"))))
    (system:command cmd (in-directory path))))

(define (src:checkout path branch)
  (let ((cmd (case (src:type path)
               ((git)
                (let ((name (or branch "master")))
                  (print:debug "src" "checkout" path name)
                  (if (src:branch-exists? path name)
                    `("git" "checkout" ,name)
                    `("git" "checkout" "-b" ,name
                      "-t" ,(string-append "origin/" name)))))
               ((hg)
                (print:debug "src" "checkout" path branch)
                "hg update -c"))))
    (system:command cmd (in-directory path))))

(define (src:discard path)
  (print:debug "src" "discard" path)
  (let ((cmd (case (src:type path)
               ((git) "git checkout .")
               ((hg)  "hg update -C"))))
    (system:command cmd (in-directory path))))

(define (src:clean path)
  (print:debug "src" "clean" path)
  (let ((cmd (case (src:type path)
               ((git) "git clean -fxd")
               ((hg)  "hg purge --all"))))
    (system:command cmd (in-directory path))))

;}}}
;{{{ packages

(define (define-package name . rest)
  (define (finalize pkg)
    (make-package (package-name    pkg)
                  (package-source  pkg)
                  (package-patches pkg)
                  (reverse (package-stages pkg))))
  (let* ((state (make-package name (make-source #f #f #f #f) '() '()))
         (pkg (apply call-with-state state rest)))
    (set! *packages* (cons (finalize pkg) *packages*))
    pkg))

(define (load-packages)
  (define (rules-file)
    (make-path (env 'lib-dir)
               (show #f (joined each (list "rules" (env 'sdk) "scm") "."))))
  (load (rules-file))
  (set! *packages* (reverse *packages*))
  *packages*)

(define (pkg:flag? name)
  (regexp-search (rx bow ,name eow) *flags*))

(define (pkg:build-root)
  (make-path (env 'build-dir) "pkg"))

(define (pkg:work-directory pkg)
  (make-path (pkg:build-root) (symbol->string (package-name pkg))))

(define (pkg:source-name pkg)
  (or (and-let* ((source (package-source pkg))
                 (location (source-location source))
                 (name (last (string-split location #\/)))
                 (r (rx (or (: (-> name (+ any)) ".git")
                            (: (-> name (+ any)) ".tar" (? "." (+ any)))
                            (: (-> name (+ any)) ".t" (+ any))
                            (-> name (+ any)))))
                 (m (regexp-matches r name)))
        (regexp-match-submatch m 'name))
      (symbol->string (package-name pkg))))

(define (pkg:source-directory pkg)
  (let* ((source (package-source pkg))
         (directory (or (and source (source-directory source))
                        (pkg:source-name pkg))))
    (or (and source
             (case (source-type source)
               ((git hg) (make-path (env 'src-dir) directory))
               (else #f)))
        (make-path (pkg:work-directory pkg) directory))))

(define (pkg:build-dir pkg)
  (pkg:source-directory pkg))

(define (pkg:scm-source? pkg)
  (and-let* ((source (package-source pkg)))
    (case (source-type source)
      ((git hg) #t)
      (else #f))))

(define (pkg:update pkg)
  (let* ((directory (pkg:source-directory pkg))
         (source    (package-source       pkg))
         (type      (source-type          source))
         (branch    (source-branch        source)))
    (case type
      ((git hg)
       (cond ((file-exists? directory)
              (src:fetch directory)
              (src:checkout directory branch)
              (src:pull directory))
             (else
               (src:clone type (source-location pkg) directory)
               (src:checkout directory branch))))
      ((dist)
       (let* ((filename (source-location source))
              (pathname (make-path (env 'dist-dir) filename)))
         (system:command `("tar" "-xf" ,pathname)
                         (in-directory (pkg:work-directory pkg)))))
      (else (error "unsupported source type" type)))))

;}}}
;{{{ messages

(define (print:message . args)
  (show #t "\x1B[1;34m:::\x1B[0m " (joined each args " ") nl))
(define (print:warning . args)
  (show #t "\x1B[1;33m:::\x1B[0m " (joined each args " ") nl))
(define (print:error . args)
  (show #t "\x1B[1;31m:::\x1B[0m " (joined each args " ") nl))
(define (print:debug . args)
  (when (string=? "yes" (env 'debug))
    (show #t "\x1B[1;36m:::\x1B[0m " (joined each args " ") nl)
    (flush-output-port)))
(define (die . args)
  (apply print:error args)
  (exit 1))

;}}}
;{{{ processes

(define (wait-result->status wait-result)
  (remainder (cadr wait-result) 255))

(define (system:run cmd . args)
  (apply print:debug cmd args)
  (wait-result->status (apply system cmd args)))

(define (system:execute cmd . args)
  (apply print:debug cmd args)
  (execute cmd (cons cmd args))
  (exit 1))

(define (wait-child pid)
  (wait-result->status (waitpid pid 0)))

(define (with-child-process thunk)
  (let ((pid (fork)))
    (cond ((zero? pid)
           (thunk)
           (exit 1))
          ((negative? pid)
           (error "fork failed"))
          (else pid))))

(define (run-process cmd . procs)
  (define (cmd->thunk)
    (cond ((string? cmd) (cut apply system:execute (string-split cmd)))
          ((list? cmd) (cut apply system:execute cmd))
          ((procedure? cmd) cmd)
          (else (error "invalid command value" cmd))))
  (with-child-process (cut (apply call-with-state (cmd->thunk) procs))))

(define (with-output-file file)
  (lambda (thunk)
    (let ((fd (open file (+ open/write open/create open/truncate))))
      (duplicate-file-descriptor-to fd 1)
      (duplicate-file-descriptor-to fd 2)
      thunk)))

(define (with-closed-stderr)
  (lambda (thunk)
    (close-file-descriptor 2)
    thunk))

(define (in-directory dir)
  (lambda (thunk)
    (or (change-directory dir)
        (die "Failed to change working directory to:" dir))
    thunk))

(define (system:command cmd . procs)
  (let ((status (wait-child (apply run-process cmd procs))))
    (unless (zero? status)
      (die "A command failed with status:" status))))

;}}}
;{{{ commands

(define (cmd:generate out-file in-file)
  (if (file-exists? out-file) (delete-file out-file))
  (with-output-to-file out-file (cut %ninja:output *packages*))
  (for-each generate-include-script *packages*))

(define (cmd:build build-file targets)
  (let ((build-dir (env 'build-dir)))
    (apply system:run "ninja" "-C" build-dir "-f" build-file targets)))

;}}}
;{{{ command: rebuild

(define (option? arg)
  (eqv? #\- (string-ref arg 0)))

(define (target-name->path name)
  (let ((build-dir (env 'build-dir)))
    (make-path build-dir name)))

(define (target-name->log-path name)
  (string-append (target-name->path name) ".log"))

(define (truncate-file pathname)
  (close-file-descriptor (open pathname (+ open/create open/truncate)))
  pathname)

(define (cmd:rebuild build-file args)
  (define-record-type <rebuild-profile>
    (make-rebuild-profile targets only show) rebuild-profile?
    (targets rebuild-targets)
    (only    rebuild-targets-only)
    (show    rebuild-show-all))

  (define (log-files targets)
    (map (cut target-name->log-path <>)
         (append targets (list "rebuild"))))

  (define (execute-tail targets)
    (define (cmd)
      (apply system:execute "tail" "-Fn+1" (log-files targets)))
    (run-process (cut cmd)
                 (with-closed-stderr)))

  (define (execute-ninja targets)
    (define (cmd)
      (apply system:execute "ninja" "-f" build-file targets))
    (run-process (cut cmd)
                 (with-output-file (target-name->log-path "rebuild"))
                 (in-directory (env 'build-dir))))

  (define (parse-args args state)
    (if (pair? args)
      (let ((t (rebuild-targets      state))
            (o (rebuild-targets-only state))
            (s (rebuild-show-all     state))
            (arg  (car args))
            (rest (cdr args)))
        (if (option? arg)
          (cond ((member arg '("-t" "--targets-only"))
                 (parse-args rest (make-rebuild-profile t #t s)))
                ((member arg '("-a" "--show-all"))
                 (parse-args rest (make-rebuild-profile t o #t)))
                (else (error "unknown rebuild option" arg)))
          (parse-args rest (make-rebuild-profile
                             (append t (list (name->target arg)))
                             o s))))
      state))

  (define (filter-stages stages name config)
    (define by-name
      (compose (cut eq? <> name) stage-name))
    (define by-config
      (compose (cut eq? <> config) stage-config))
    (if config
      (filter (conjoin by-name by-config) stages)
      (filter by-name stages)))

  (define (stage->target pkg stage)
    (make-target (package-name pkg)
                 (stage-name stage)
                 (stage-config stage)))

  (define (real-targets target)
    (let ((n (target-name target))
          (s (or (target-stage target) 'build))
          (c (target-config target)))
      (and-let* ((pkg (find-package n))
                 (stages (package-stages pkg)))
        (map (cut stage->target pkg <>)
             (filter-stages stages s c)))))

  (define (rebuild profile)
    (let* ((rts (rebuild-targets      profile))
           (targets-only (rebuild-targets-only profile))
           (rsa (rebuild-show-all     profile))
           (targets (map target->string (concatenate (map real-targets rts)))))
      (for-each (lambda (f) (if (file-exists? f) (delete-file f)))
                (append (map (cut target-name->path <>) targets)
                        (map (cut target-name->log-path <>) targets)))
      (let* ((tail-pid (execute-tail targets))
             (ninja-pid (if targets-only
                          (execute-ninja targets)
                          (execute-ninja '())))
             (status (wait-child ninja-pid)))
        (kill tail-pid signal/term)
        (wait-child tail-pid)
        status)))

  (rebuild (parse-args args (make-rebuild-profile '() #f #f))))

;}}}
;{{{ command: src

(define (cmd:src args)
  (define (scm-packages names)
    (let ((ids (map string->symbol names))
          (scms (filter pkg:scm-source? *packages*)))
      (if (pair? names)
        (filter (lambda (pkg)
                  (let ((name (package-name pkg)))
                    (any (cut eq? name <>) ids)))
                scms)
        scms)))
  (define (print-status pkg)
    (and-let* ((n (package-name pkg))
               (s (pkg:source-directory pkg)))
      (show #t n ": " (src:head s) (if (src:dirty? s) " dirty" "") nl)))
  (match args
    (("status" names ...)
     (for-each print-status (scm-packages names)))
    (("update" names ...)
     (for-each pkg:update (map find-package names)))
    (other (die "unsupported subcommand:" other)))
  0)

;}}}
;{{{ command: exec

(define (name->target name)
  (define (name->list name)
    (let ((ls (map (lambda (x) (and (not (string-null? x)) x))
                   (string-split name #\:))))
      (case (length ls)
        ((1) (list (car ls) #f #f))
        ((2) (list (car ls) (cadr ls) #f))
        ((3) ls)
        (else (error "invalid target name" name)))))
  (match (name->list name)
    ((n s c)
     (if (not n) (error "empty target name" name)
       (make-target (string->symbol n)
                    (and s (string->symbol s))
                    (and c (string->symbol c)))))))

(define (find-package name)
  (let ((id (if (symbol? name) name (string->symbol name))))
    (find (compose (cut eq? <> id) package-name) *packages*)))

(define (exec:stage target)
  (show #t target " -> " nl))

(define (cmd:exec args)
  (for-each exec:stage (map name->target args)))

;}}}
;{{{ main

(define (main arguments)
  (load-packages)
  (match arguments
    ((_ "generate" out in)
     (cmd:generate out in))
    ((_ "build" build-file targets ...)
     (exit (cmd:build build-file targets)))
    ((_ "rebuild" build-file args ...)
     (exit (cmd:rebuild build-file args)))
    ((_ "src" args ...)
     (exit (cmd:src args)))
    ((_ "exec" args ...)
     (exit (cmd:exec args)))
    ((_ cmd args ...)
     (die "unknown command:" cmd))))

;}}}
