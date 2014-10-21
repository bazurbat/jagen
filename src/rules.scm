(%include "src/rules.ninja")

(when (env 'sdk)
  (load (string-append "src/rules." (env 'sdk) ".scm")))
