(cl:defpackage #:trivial-jumptables
  (:nicknames #:jumpcase)
  (:use #:cl)
  (:export #:ejumpcase

           #:*environment*
           #:*ejumpcase-expander*
           #:expand-ejumpcase-linear
           #:expand-ejumpcase-logarithmic
           #:index
           #:*type-annotate-index-form*
           #:maybe-type-annotate-index-form

           #:*vectorize*
           #:*vectorization-threshold*
           #:*vectorization-threshold-function*
           #:standard-vectorization-threshold
           #:make-standard-vectorizing-wrapper

           #:*precompute-constant-index*
           #:*preselect-case*
           #:make-standard-optimizations-wrapper))
