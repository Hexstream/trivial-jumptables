(asdf:defsystem #:trivial-jumptables

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides efficient O(1) jump tables on supported Common Lisp implementations and falls back to O(log(n)) on others. Important optimizations are performed even on unsupported implementations, notably \"vectorization\" which allows O(1) dispatch if all cases are constant."

  :depends-on ()

  :version "1.1"
  :serial cl:t
  :components ((:file "package")
               (:file "vectorize")
               (:file "optimize")
	       (:file "expand"))

  :in-order-to ((asdf:test-op (asdf:test-op #:trivial-jumptables_tests))))
