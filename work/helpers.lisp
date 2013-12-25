(defun partial (func &rest args1)
  (lambda (&rest args2) (apply func (append args1 args2))))
