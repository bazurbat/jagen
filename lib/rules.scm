(%variable "builddir" (env 'build-dir))
(show #t nl)

(define *flags* (or (env 'flags) ""))

(%rule (make-rule
         "command"
         (list (cons "command" "$command"))))

(%rule (make-rule
         "script"
         (list (cons "command"
                     (string-append (env 'bin-dir)
                                    "/$script && touch $out")))))

(when (env 'sdk)
  (load (string-append
          (env 'lib-dir) "/rules." (env 'sdk) ".scm")))
