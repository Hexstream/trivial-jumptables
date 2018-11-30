(in-package #:trivial-jumptables_tests)

(defun %default-example-case-generator (i)
  (let* ((symbol-name (format nil "~R" i))
         (symbol (find-symbol symbol-name)))
    (if symbol
        (values `',symbol
                nil)
        (values `',(read-from-string (substitute #\- #\Space symbol-name))
                (lambda ()
                  (unintern symbol))))))

(defparameter *example-case-generator* #'%default-example-case-generator)

(defun %generate-example-lambda (n &optional (case-generator *example-case-generator*))
  (let ((cleanups nil))
    (values `(lambda (index)
               (ejumpcase index
                 ,@(map-into (make-list n)
                             (let ((i 0))
                               (lambda ()
                                 (prog1 (multiple-value-bind (form cleanup)
                                            (funcall case-generator i)
                                          (when cleanup
                                            (push cleanup cleanups))
                                          form)
                                   (incf i)))))))
            (lambda ()
              (mapc #'funcall (nreverse cleanups))))))

(defun disassemble-example (&key (n 5) (case-generator *example-case-generator*) (print-form t)
                              ((:ejumpcase-expander jumpcase:*ejumpcase-expander*) jumpcase:*ejumpcase-expander*))
  (let ((*package* (find-package '#:trivial-jumptables_tests)))
    (multiple-value-bind (lambda-form cleanup)
        (%generate-example-lambda n case-generator)
      (disassemble lambda-form)
      (when print-form
        (format t "~%Above is the disassembly for form:~%~S" lambda-form))
      (funcall cleanup)
      (values))))

(defun benchmark (&key (n 1000) (repeat 1000000) (case-generator *example-case-generator*) (print-form nil)
                    ((:ejumpcase-expander jumpcase:*ejumpcase-expander*) jumpcase:*ejumpcase-expander*))
  (format t "Preparing to execute a ~A entries jumptable ~A times."
          n repeat)
  (let ((*package* (find-package '#:trivial-jumptables_tests)))
    (multiple-value-bind (lambda-form cleanup)
        (%generate-example-lambda n case-generator)
      (format t "~%Compiling test.")
      (let ((compiled (compile nil lambda-form)))
        (format t "~%Running test.")
        (time (dotimes (i repeat)
                (funcall compiled (mod i n)))))
      (when print-form
        (format t "~%Above is the benchmark test results for form:~%~S" lambda-form))
      (funcall cleanup)
      (values))))
