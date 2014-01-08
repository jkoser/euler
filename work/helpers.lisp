(defun partial (func &rest args1)
  "use cl-op:op instead of this"
  (lambda (&rest args2) (apply func (append args1 args2))))

(defmacro multf (place multiplier)
  `(setf ,place (* ,place ,multiplier)))

(defmacro dolist-cross ((var1 list1 var2 list2 &optional result) &rest body)
  `(dolist (,var1 ,list1 ,result)
     (dolist (,var2 ,list2)
       ,@body)))

(defun primes-below (n)
  "returns a bit array indicating truth values from 0 to n - 1"
  (declare (fixnum n))
  (let ((prime (make-array n :element-type 'bit :initial-element 1)))
    (if (> n 0) (setf (sbit prime 0) 0))
    (if (> n 1) (setf (sbit prime 1) 0))
    (do ((i 3 (+ i 1)))
        ((>= i n))
      (declare (fixnum i))
      (cond ((evenp i)
             (setf (sbit prime i) 0))
            ((not (zerop (sbit prime i)))
             (do ((i2 (* i 2))
                  (j (* i 3) (+ j i2)))
                 ((>= j n))
               (declare (fixnum i2 j))
               (setf (sbit prime j) 0)))))
    prime))

(defun primes-below-list (n)
  "returns a list of all primes strictly less than n"
  (iter (for b in-vector (primes-below n) with-index i)
        (if (zerop b) (next-iteration))
        (collect i)))

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
