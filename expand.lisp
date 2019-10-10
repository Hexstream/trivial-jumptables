(in-package #:trivial-jumptables)

(deftype jumpcase:index (case-count)
  `(integer 0 (,case-count)))

(defparameter jumpcase:*type-annotate-index-form* t)

(defun jumpcase:maybe-type-annotate-index-form (index-form case-count)
  (if *type-annotate-index-form*
      `(the (jumpcase:index ,case-count) ,index-form)
      index-form))

(defun jumpcase:expand-ejumpcase-linear (index-form cases case-count)
  `(ecase ,(maybe-type-annotate-index-form index-form case-count)
     ,@(mapcar (let ((i 0))
                 (lambda (form)
                   (prog1 `(,i ,form)
                     (incf i))))
               cases)))

(defun jumpcase:expand-ejumpcase-logarithmic (index-form cases case-count)
  (let ((index-var (gensym (string '#:index)))
        (cases (coerce cases 'vector)))
    (assert (= (length cases) case-count))
    (labels ((recurse (offset length)
               (if (= length 1)
                   (svref cases offset)
                   (let* ((half-length (ceiling length 2))
                          (midpoint (+ offset half-length)))
                     `(if (< ,index-var ,midpoint)
                          ,(recurse offset half-length)
                          ,(recurse midpoint (- length half-length)))))))
      (let* ((index-type `(jumpcase:index ,case-count))
             (error-form `(error 'type-error
                                 :datum ,index-var
                                 :expected-type ',index-type)))
        `(let ((,index-var ,index-form))
           ,(if (plusp case-count)
                `(if (typep ,index-var ',index-type)
                     ,(recurse 0 case-count)
                     ,error-form)
                error-form))))))

(defparameter jumpcase:*ejumpcase-expander*
  #-ccl
  (jumpcase:make-standard-optimizations-wrapper
   'jumpcase:expand-ejumpcase-logarithmic)
  #+ccl
  (jumpcase:make-standard-optimizations-wrapper
   'jumpcase:expand-ejumpcase-linear))

(defvar *%initial-ejumpcase-expander* jumpcase:*ejumpcase-expander*)

(defmacro ejumpcase (index &body cases &environment env)
  (let ((jumpcase:*environment* env)) (funcall jumpcase:*ejumpcase-expander* index cases (length cases))))
