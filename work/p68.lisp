(use-package :cl-op)
(use-package :cl-op.hof)

(defun magicalp (lines)
  (let ((sums (mapcar
	       (op apply #'+ _)
	       (remove-if (op member-if #'symbolp _) lines))))
    (or (null sums) (apply #'= sums))))

(defun count-magic-solutions (lines)
  (labels ((rec (lines syms vals)
	     (cond ((not (magicalp lines)) 0)
		   ((null syms) 1)
		   (t (apply
		       #'+
		       (mapcar (lambda (val)
				 (rec (subst val (car syms) lines)
				      (cdr syms)
				      (remove val vals)))
			       vals))))))
    (let ((syms (remove-duplicates
		 (remove-if-not #'symbolp (flatten lines)))))
      (rec lines syms
	   (loop for i from 1 to (list-length syms) collect i)))))

(defun magic-solutions (lines)
  (labels ((rec (lines syms vals)
	     (cond ((not (magicalp lines)) nil)
		   ((null syms) (list lines))
		   (t (apply
		       #'nconc
		       (mapcar (lambda (val)
				 (rec (subst val (car syms) lines)
				      (cdr syms)
				      (remove val vals)))
			       vals))))))
    (let ((syms (remove-duplicates
		 (remove-if-not #'symbolp (flatten lines)))))
      (rec lines syms
	   (loop for i from 1 to (list-length syms) collect i)))))

(defun solve-3-gon ()
  (count-magic-solutions '((a b c) (d c e) (f e b))))

(defun solve-5-gon ()
  (let ((all (magic-solutions
	      '((a b c) (d c e) (f e g) (h g i) (j i b))))
	;; we only want 16-digit solutions
	(sixteen-digit
	 (lambda (lines) (= (count 10 (flatten lines)) 1)))
	;; unique solutions start with lowest external node
	(lowest-first
	 (lambda (lines)
		     (let ((first (caar lines)))
		       (every (op < first (car _)) (cdr lines))))))
    (delete-if-not (conjoin sixteen-digit lowest-first) all)))
