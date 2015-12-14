(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr)
                   expr
                   (expand-call type (binarize expr)))))

(defun expand-call (type expr)
  `(,(car expr) ,@(mapcar #'(lambda (a)
                              `(with-type ,type ,a))
                          (cdr expr))))

(defun binarize (expr)
  (if (and (nthcdr 3 expr)
           (member (car expr) '(+ - * /)))
      (destructuring-bind (op a1 a2 . rest) expr
        (binarize `(,op (,op ,a1 ,a2) ,@rest)))
      expr))

(defun right-triangles (len)
  (declare (type fixnum len))
  (declare (optimize (speed 3) (safety 0)))
  (loop for c fixnum from (ceiling len 3) to (floor len 2)
     nconc
       (loop with ab fixnum = (- len c)
            for a fixnum from 1 to (floor ab 2)
            when (= (the fixnum (expt c 2))
                    (with-type fixnum (+ (expt a 2) (expt (- ab a) 2))))
            collect (list a (- ab a) c))))

(defun distinct-right-triangles (maxlen)
  (do ((sieve (make-array maxlen :element-type 'fixnum :initial-element 0))
       (n 12 (+ n 2))
       (triangles nil)
       (singles 0))
      ((>= n maxlen)
       (values (nreverse triangles) singles))
    (if (zerop (aref sieve n))
        (let ((ts (right-triangles n)))
          (if (consp ts)
              (progn
                (push (cons n ts) triangles)
                (do ((m n (+ m n)))
                    ((>= m maxlen))
                  (incf (aref sieve m)))))))
    (if (= (aref sieve n) 1)
        (incf singles))))

