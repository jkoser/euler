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

(format t "~A~%" (nth 999999 (permute '(0 1 2 3 4 5 6 7 8 9))))
