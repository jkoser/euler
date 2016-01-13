#!/usr/bin/sbcl --script

(defun permute (lst)
  (if (null lst)
    '(())
    (apply #'append
           (do ((x (car lst) (car remaining))
                (previous nil (cons x previous))
                (remaining (cdr lst) (cdr remaining))
                (ps nil (cons (mapcar (lambda (y) (cons x y))
                                      (permute (append (reverse previous) remaining)))
                              ps)))
               ((null x) (reverse ps))))))

(defun factorial (n)
  (do ((n n (- n 1))
       (p 1 (* p n)))
      ((<= n 0) p)))

(defun nth-permutation (n lst)
  (labels ((f (n lst rprefix)
             (if (null lst)
               (reverse rprefix)
               (let* ((k (length lst))
                      (s (factorial (- k 1)))
                      (i (floor n s))
                      (r (mod n s)))
                 (if (>= i k)
                   nil
                   (f r
                      (append (subseq lst 0 i) (subseq lst (+ i 1)))
                      (cons (nth i lst) rprefix)))))))
    (f n lst nil)))

(format t "~A~%" (nth-permutation 999999 '(0 1 2 3 4 5 6 7 8 9)))
