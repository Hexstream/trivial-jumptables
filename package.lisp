(cl:defpackage #:trivial-jumptables
  (:nicknames #:jumpcase)
  (:use #:cl)
  (:export #:ejumpcase

           #:*ejumpcase-expander*
           #:expand-ejumpcase-linear
           #:expand-ejumpcase-logarithmic
           #:index
           #:*type-annotate-index-form*
           #:maybe-type-annotate-index-form

           #:*precompute-constant-index*
           #:*preselect-case*
           #:make-standard-optimizations-wrapper))
