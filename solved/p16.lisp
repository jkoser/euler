#!/usr/bin/sbcl --script

(defun digit-sum (n)
  (if (< n 1)
      0
      (multiple-value-bind (d r) (floor n 10)
        (+ r (digit-sum d)))))

(princ (digit-sum (expt 2 1000)))
(terpri)
