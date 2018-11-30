(asdf:defsystem #:trivial-jumptables_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "trivial-jumptables unit tests."

  :depends-on ("trivial-jumptables"
               "parachute"
               "bubble-operator-upwards")

  :serial cl:t
  :components ((:file "tests")
               (:file "repl"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:trivial-jumptables_tests)))
