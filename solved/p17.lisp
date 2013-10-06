#!/usr/bin/sbcl --script

(defun digit-word (d)
  (case d
    (0 "zero")
    (1 "one")
    (2 "two")
    (3 "three")
    (4 "four")
    (5 "five")
    (6 "six")
    (7 "seven")
    (8 "eight")
    (9 "nine")))

(defun two-digit-prefix (d)
  (case d
    (0 "zero")
    (1 "ten")
    (2 "twenty")
    (3 "thirty")
    (4 "forty")
    (5 "fifty")
    (6 "sixty")
    (7 "seventy")
    (8 "eighty")
    (9 "ninety")))

(defun two-digit-words (n)
  (if (< n 10)
    (digit-word n)
    (case n
      (10 "ten")
      (11 "eleven")
      (12 "twelve")
      (13 "thirteen")
      (14 "fourteen")
      (15 "fifteen")
      (16 "sixteen")
      (17 "seventeen")
      (18 "eighteen")
      (19 "nineteen")
      (otherwise
        (multiple-value-bind (q r) (floor n 10)
          (if (zerop r)
            (two-digit-prefix q)
            (format nil "~A-~A" (two-digit-prefix q) (digit-word r))))))))

(defun number-words (n)
  (cond ((< n 100)
         (two-digit-words n))
        ((= n 1000)
         "one thousand")
        ((zerop (mod n 100))
         (format nil "~A hundred" (digit-word (/ n 100))))
        (t
         (format nil "~A hundred and ~A" (digit-word (floor n 100)) (two-digit-words (mod n 100))))))

(defun string-letters (s)
  (let ((len (length s)))
    (do ((i 0 (+ i 1))
         (sum 0 (+ sum (if (alpha-char-p (char s i)) 1 0))))
        ((>= i len) sum))))

(format t "~A~%"
        (do ((i 1 (+ i 1))
             (sum 0 (+ sum (string-letters (number-words i)))))
            ((> i 1000) sum)))
