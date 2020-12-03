;;;; Day 3 - TBD
;;;; I should really make a template containing code I'll probably need

(ql:quickload '(:cl-ppcre :alexandria))
(declaim (optimize (speed 0) (safety 0) (debug 3)))

(defun read-input (filename)
  (uiop:read-file-lines filename))

(defun day3-solver (filename movement &optional (height #'cdr))
  (let* ((input (read-input filename))
         (width (length (first input)))
         (pos 0)
         (hits 0))
   (loop :for row :in input :by height doing
      (when (char= (char row pos) #\#)
        (setf hits (1+ hits)))
      (setf pos (mod (+ movement pos) width))
      :finally (return hits))))

(day3-solver "day3-test" 1) ;-> 2
(day3-solver "day3-test" 3) ;-> 7
(day3-solver "day3-test" 5) ;-> 3
(day3-solver "day3-test" 7) ;-> 4

(*
 (day3-solver "day3-input" 1) ;-> 84
 (day3-solver "day3-input" 3) ;-> 195
 (day3-solver "day3-input" 5) ;-> 70
 (day3-solver "day3-input" 7) ;-> 70
 (day3-solver "day3-input" 1 #'cddr)) ;-> 47, 3772314000
