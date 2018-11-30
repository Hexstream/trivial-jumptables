(in-package #:trivial-jumptables)

(defparameter jumpcase:*precompute-constant-index* t)

(defun %maybe-precompute-index (index-form)
  (if (and jumpcase:*precompute-constant-index* (constantp index-form))
      (eval index-form)
      index-form))

(defparameter jumpcase:*preselect-case* t)

(defun %maybe-preselect-case-index (index-form case-count)
  (when (and jumpcase:*preselect-case*
             (typep index-form `(jumpcase:index ,case-count)))
    index-form))

(defun jumpcase:make-standard-optimizations-wrapper (ejumpcase-expander)
  (lambda (index-form cases case-count)
    (setf index-form (%maybe-precompute-index index-form))
    (let ((preselected-case-index (%maybe-preselect-case-index index-form case-count)))
      (if preselected-case-index
          (nth preselected-case-index cases)
          (funcall ejumpcase-expander index-form cases case-count)))))
