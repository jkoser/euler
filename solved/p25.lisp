#!/usr/bin/sbcl --script

(do ((n 2 (+ n 1))
     (fn 1 (+ fn fnm1))
     (fnm1 1 fn))
    ((>= (length (format nil "~A" fn)) 1000)
     (format t "~A" n)))
