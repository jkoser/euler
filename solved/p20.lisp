#!/usr/bin/sbcl --script

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))

(defun digit-sum (n)
  (if (< n 1)
      0
      (multiple-value-bind (d r) (floor n 10)
        (+ r (digit-sum d)))))

(princ (digit-sum (fact 100)))
(terpri)
