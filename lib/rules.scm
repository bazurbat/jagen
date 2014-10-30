(%include "lib/rules.ninja")

(when (env 'sdk)
  (load (string-append "lib/rules." (env 'sdk) ".scm")))
