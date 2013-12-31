(defun solve-p70 ()
  (let* ((limit 1000000)
         (totients (totients-below limit))
         (permutations (loop for i from 2 upto (- limit 1)
                          if (digit-permutation-p (aref totients i) i)
                          collect (cons i (/ i (aref totients i))))))
    (reduce (lambda (a b) (if (< (cdr a) (cdr b)) a b))
            permutations :initial-value (cons -1 2))))

