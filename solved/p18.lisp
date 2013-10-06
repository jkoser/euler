#!/usr/bin/sbcl --script

(defun read-triangle (fname)
  (with-open-file (in fname)
    (do ((line (read-line in nil)
               (read-line in nil))
         (triangle nil (cons (with-input-from-string (s line)
                               (do ((o (read s nil)
                                       (read s nil))
                                    (lst nil (cons o lst)))
                                   ((null o) (nreverse lst))))
                             triangle)))
        ((null line) (nreverse triangle)))))

;;; returns the max path from top to bottom of the triangle (in list form)
(defun max-path (triangle)
  (let* ((side (length triangle))
         ;; array form of the input triangle
         (tri-arr (make-array (list side side)))
         ;; array holding max paths from top to locations
         (sum-arr (make-array (list side side))))
    ;; convert the list triangle to array form
    (do ((rows triangle (cdr rows))
         (i 0 (+ i 1)))
        ((null rows))
        (do ((cells (car rows) (cdr cells))
             (j 0 (+ j 1)))
            ((null cells))
            (setf (aref tri-arr i j) (car cells))))
    ;; the max path of the triangle's tip is itself
    (setf (aref sum-arr 0 0) (aref tri-arr 0 0))
    ;; process subsequent rows
    (do ((i 1 (+ i 1)))
        ((>= i side))
        ;; left edge
        (setf (aref sum-arr i 0) (+ (aref sum-arr (- i 1) 0)
                                    (aref tri-arr i 0)))
        ;; middle
        (do ((j 1 (+ j 1)))
            ((>= j i))
            (setf (aref sum-arr i j) (+ (max (aref sum-arr (- i 1) (- j 1))
                                             (aref sum-arr (- i 1) j))
                                        (aref tri-arr i j))))
        ;; right edge
        (setf (aref sum-arr i i) (+ (aref sum-arr (- i 1) (- i 1))
                                    (aref tri-arr i i))))
    ;; return the greatest value in the last row of paths
    (let ((last-row (make-array side
                       :displaced-to sum-arr
                       :displaced-index-offset
                         (array-row-major-index sum-arr (- side 1) 0))))
      (apply #'max (map 'list #'identity last-row)))))

(format t "~A~%" (max-path (read-triangle "p18.txt")))
