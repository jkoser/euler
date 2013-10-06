#!/usr/bin/sbcl --script

(defun divisor-sum (n)
  (do ((k 2 (+ k 1))
       (s 1 (if (zerop (mod n k))
              (if (= (* k k) n)
                (+ s k)
                (+ s k (/ n k)))
              s)))
      ((>= (* k k) n) s)))

(defun amicable-sum (n)
  (do ((i 1 (+ i 1))
       (s 0 (let ((ds (divisor-sum i)))
              (if (and (/= i ds) (= i (divisor-sum ds)))
                (+ s i)
                s))))
      ((> i n) s)))

(format t "~A~%" (amicable-sum 10000))
