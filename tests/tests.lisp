(cl:defpackage #:trivial-jumptables_tests
  (:nicknames #:jumpcase_tests)
  (:use #:cl #:parachute)
  (:import-from #:jumpcase #:ejumpcase)
  (:import-from #:bubble-operator-upwards #:cartesian-product)
  (:export #:disassemble-example #:benchmark))

(cl:in-package #:trivial-jumptables_tests)

(defmacro run-tests (&body body)
  ;; This following form makes tests super slow.
  (setf body (append body (let ((casecount 100)
                                (cases nil))
                            (dotimes (i casecount)
                              (push (- i) cases))
                            (setf cases (nreverse cases))
                            (list `(is equal ',cases
                                       (let ((results nil))
                                         (flet ((compute (index)
                                                  (ejumpcase index
                                                    ,@cases)))
                                           (dotimes (i ,casecount)
                                             (push (compute i) results))
                                           (nreverse results))))))))
  (flet ((compile-it (expander precompute-constant-index preselect-case type-annotate-index-form)
           (let ((settings `((jumpcase:*ejumpcase-expander* ,expander)
                             (jumpcase:*precompute-constant-index* ,precompute-constant-index)
                             (jumpcase:*preselect-case* ,preselect-case)
                             (jumpcase:*type-annotate-index-form* ,type-annotate-index-form))))
             `(progn
                (format t "~%Now running tests for settings: ~S" ',settings)
                (let ,settings
                  (funcall (compile nil '(lambda () ,@body))))))))
    `(progn
       ,@(mapcar (lambda (combination)
                   (apply #'compile-it combination))
                 (cartesian-product '('jumpcase:expand-ejumpcase-linear
                                      'jumpcase:expand-ejumpcase-logarithmic
                                      (jumpcase:make-standard-optimizations-wrapper
                                       'jumpcase:expand-ejumpcase-linear)
                                      (jumpcase:make-standard-optimizations-wrapper
                                       'jumpcase:expand-ejumpcase-logarithmic)
                                      jumpcase::*%initial-ejumpcase-expander*)
                                    '(t nil) '(t nil) '(t nil))))))

(defun %wat ()
  (error "Wat??"))

(define-test "main"
  (run-tests
    (fail (ejumpcase 0) 'type-error)
    (is eq 'zero
        (ejumpcase 0
          'zero))
    (is eq 'zero
        (ejumpcase 0
          'zero
          'one))
    (is eq 'one
        (ejumpcase 1
          'zero
          'one))
    (fail (ejumpcase -1
            'zero)
          'type-error)
    (fail (ejumpcase 0.0
            'zero)
          'type-error)
    (fail (ejumpcase 1
            'zero)
          'type-error)
    (fail (ejumpcase '(%wat)
            'zero
            'one))
    (is eq 'four
        (ejumpcase 4
          'zero
          'one
          'two
          'three
          'four))
    (is eq 'four
        (ejumpcase 4
          'zero
          'one
          'two
          'three
          'four
          'five))
    (is eq 'two
        (ejumpcase 2
          'zero
          'one
          'two
          'three
          'four
          'five
          'six
          'seven
          'eight
          'nine
          'ten))))
