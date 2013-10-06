#!/usr/bin/sbcl --script

(defun days-in-month (year month)
  (cond ((= month 2)
         (if (zerop (mod year 4))
           (if (zerop (mod year 100))
             (if (zerop (mod year 400)) 29 28)
             29)
           28))
        ((member month '(4 6 9 11)) 30)
        (t 31)))

(defun sunday-firsts ()
  (do ((y 1900 (if (= m 12) (+ y 1) y))
       (m 1 (+ (mod m 12) 1))
       (dow 1 (mod (+ dow (days-in-month y m)) 7))
       (c 0 (if (and (>= y 1901) (= dow 0)) (+ c 1) c)))
      ((> y 2000) c)))

(format t "~A~%" (sunday-firsts))
