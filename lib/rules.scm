(%variable "builddir" (env 'build-dir))
(show #t nl)

(define *flags* (or (env 'flags) ""))

(%rule (make-rule
         "command"
         (list (cons "command" "$command"))))

(%rule (make-rule
         "script"
         (list (cons "command" "bin/$script && touch $out"))))

(when (env 'sdk)
  (load (string-append "lib/rules." (env 'sdk) ".scm")))
