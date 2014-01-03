(defun partial (func &rest args1)
  "use cl-op:op instead of this"
  (lambda (&rest args2) (apply func (append args1 args2))))

(defmacro multf (place multiplier)
  `(setf ,place (* ,place ,multiplier)))

(defun totients-below (n)
  "returns an array of totients from 2 to n - 1"
  (declare (optimize (speed 3)))
  (let ((totients (make-array n :element-type 'fixnum :initial-element 1)))
    (do ((i 2 (1+ i)))
	((= i n))
      (declare (type fixnum i))
      (if (= (aref totients i) 1) ; prime
          (progn
            ;; multiples of prime
            (do ((j i (+ j i)))
                ((>= j n))
              (declare (type fixnum j))
              (multf (aref totients j) (1- i)))
            ;; multiples of powers of prime
            (do ((ix (* i i) (* ix i)))
                ((>= ix n))
              (declare (type fixnum ix))
              (do ((j ix (+ j ix)))
                  ((>= j n))
                (declare (type fixnum j))
                (multf (aref totients j) i))))))
    totients))

(defun digit-counts (n)
  (declare (fixnum n))
  (declare (optimize (speed 3)))
  (let ((ds (make-array 10 :element-type 'fixnum :initial-element 0)))
    (if (= n 0)
        (setf (aref ds 0) 1)
        (do ((q n) (r 0))
            ((= q 0))
          (declare (fixnum q r))
          (multiple-value-setq (q r) (floor q 10))
          (incf (aref ds r))))
    ds))

(defun digit-permutation-p (x y)
  (equalp (digit-counts x) (digit-counts y)))
