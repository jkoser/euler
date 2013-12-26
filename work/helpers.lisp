(defun partial (func &rest args1)
  "use cl-op:op instead of this"
  (lambda (&rest args2) (apply func (append args1 args2))))

(defmacro multf (place multiplier)
  `(setf ,place (* ,place ,multiplier)))

(defun totients-below (n)
  "returns an array of totients from 2 to n - 1"
  (let ((totients (make-array n :initial-element 1)))
    (do ((i 2 (1+ i)))
	((= i n))
      (if (= (svref totients i) 1) ; prime
	  ;; multiples of prime
	  (do ((j i (+ j i)))
	      ((>= j n))
	    (multf (svref totients j) (1- i)))
	  ;; multiples of powers of prime
	  (do ((ix (* i i) (* ix i)))
	      ((>= ix n))
	    (do ((j ix (+ j ix)))
		((>= j n))
	      (multf (svref totients j) i)))))
    totients))

(defun digit-permutation-p (x y)
  (equal (sort (write-to-string x) #'char<)
	 (sort (write-to-string y) #'char<)))
