(%include "lib/jagen/rules.ninja")

(when (env 'sdk)
  (load (string-append "lib/jagen/rules." (env 'sdk) ".scm")))
