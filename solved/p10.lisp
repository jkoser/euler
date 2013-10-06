#!/usr/bin/sbcl --script

(defun sum-primes (n)
  (let ((sum 0)
        (sieve (make-array n :initial-element nil)))
    (do ((i 2 (+ i 1)))
        ((>= i n))
      (if (null (aref sieve i))
          (progn
            (setf sum (+ sum i))
            (do ((j (* 2 i) (+ j i)))
                ((>= j n))
              (setf (aref sieve j) 't)))
          nil))
    sum))

(princ (sum-primes 2000000))
(terpri)
