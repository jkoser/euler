(defun digit-factorial-chain (n)
  (labels ((next (n)
             (reduce #'+ (mapcar #'factorial (digits n)))))
    (do ((seen (make-hash-table))
         (chain nil (cons n chain))
         (n n (next n)))
        ((gethash n seen)
         (nreverse chain))
      (setf (gethash n seen) t))))

(defun problem-74 ()
  (let ((cache (make-hash-table)))
    (labels ((next (n)
               (reduce #'+ (mapcar #'factorial (digits n))))
             (chain-length (n)
               (cachehash n cache
                          (let ((m (next n)))
                            (if (= m n)
                                1
                                (1+ (chain-length m)))))))
      (loop for n in '(169 363601 1454) do (setf (gethash n cache) 3))
      (loop for n in '(871 45361 871 45362) do (setf (gethash n cache) 2))
      (loop for n from 1 to 1000000
           count (= (chain-length n) 60)))))

