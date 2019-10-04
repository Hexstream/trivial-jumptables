(asdf:defsystem #:trivial-jumptables

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides efficient O(1) jump tables on supported Common Lisp implementations and falls back to O(log(n)) on others."

  :depends-on ()

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
               (:file "optimizations")
	       (:file "main"))

  :in-order-to ((asdf:test-op (asdf:test-op #:trivial-jumptables_tests))))
