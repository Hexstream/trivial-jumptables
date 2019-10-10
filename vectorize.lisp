(in-package #:trivial-jumptables)

(defvar jumpcase:*environment* nil)

(defparameter jumpcase:*vectorize* #-ccl t #+ccl nil)

(defparameter jumpcase:*vectorization-threshold* 2/3)

(defun jumpcase:standard-vectorization-threshold (case-count)
  (let ((threshold jumpcase:*vectorization-threshold*))
    (check-type threshold (real 0 1))
    (round (* threshold case-count))))

(defparameter jumpcase:*vectorization-threshold-function*
  #'jumpcase:standard-vectorization-threshold)

(defun %fill-constantp-vector (constantp-vector cases threshold)
  (let ((fail-threshold (- (length constantp-vector) threshold))
        (non-constant-count 0)
        (i 0)
        (env jumpcase:*environment*))
    (dolist (case cases (if (zerop non-constant-count)
                            :full-vectorize
                            :partial-vectorize))
      (let ((constantp (constantp case env)))
        (setf (sbit constantp-vector i) (if constantp 1 0))
        (unless constantp
          (incf non-constant-count)
          (when (> non-constant-count fail-threshold)
            (return :no-vectorize))))
      (incf i))))

(defun %call-threshold-function (threshold-function case-count)
  (let ((threshold (funcall threshold-function case-count)))
    (if (typep threshold `(integer 0 ,case-count))
        threshold
        (error "~S ~S returned invalid threshold ~S given case-count of ~S."
               'jumpcase:*vectorization-threshold-function*
               jumpcase:*vectorization-threshold-function*
               threshold
               case-count))))

(defun %partial-vectorize (ejumpcase-expander index-form cases constantp-vector)
  (let ((value-var (gensym (string '#:value)))
        (non-constant-cases nil)
        (non-constant-cases-count 0))
    `(let ((,value-var
            (svref ,(map 'vector (lambda (case constantp)
                                   (if (= constantp 1)
                                       (eval case)
                                       (prog1 (cons 'must-eval non-constant-cases-count)
                                         (push case non-constant-cases)
                                         (incf non-constant-cases-count))))
                         cases
                         constantp-vector)
                   ,index-form)))
       (if (and (consp ,value-var)
                (eq (car ,value-var) 'must-eval))
           ,(funcall ejumpcase-expander
                     `(cdr ,value-var)
                     (nreverse non-constant-cases)
                     non-constant-cases-count)
           ,value-var))))

(defun jumpcase:make-standard-vectorizing-wrapper
    (ejumpcase-expander &key (threshold-function
                              (lambda (case-count)
                                (funcall jumpcase:*vectorization-threshold-function* case-count))))
  (lambda (index-form cases case-count)
    (if (and jumpcase:*vectorize* (>= case-count 2))
        (let ((constantp-vector (make-array case-count :element-type 'bit)))
          (declare (dynamic-extent constantp-vector))
          (ecase (%fill-constantp-vector constantp-vector
                                         cases
                                         (%call-threshold-function threshold-function case-count))
            (:no-vectorize
             (funcall ejumpcase-expander index-form cases case-count))
            (:partial-vectorize
             (%partial-vectorize ejumpcase-expander index-form cases constantp-vector))
            (:full-vectorize
             `(svref ,(map 'vector #'eval cases) ,index-form))))
        (funcall ejumpcase-expander index-form cases case-count))))
