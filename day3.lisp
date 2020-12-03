;;;; Day 3 - TBD
;;;; I should really make a template containing code I'll probably need

(ql:quickload '(:iterate))
(declaim (optimize (speed 0) (safety 0) (debug 3)))

(use-package :iterate)

(defconstant +input+ (coerce (uiop:read-file-lines "day3-input") 'vector))
(defconstant +test+ (coerce (uiop:read-file-lines "day3-test") 'vector))

(defun day3-solver-loop (input x &optional (y 1))
  (let ((height (length input))
        (width  (length (aref input 0))))
    (loop :for row :from 0 :below height :by y
          :for col = 0 :then (mod (+ col x) width)
          :count (char= (char (aref input row) col) #\#))))

(defun day3-solver-iterate (input x &optional (y 1))
  (let ((height (length input))
        (width  (length (aref input 0))))
    (iter
      (for row from 0 below height by y)
      (for col initially 0 then (mod (+ col x) width))
      (counting (char= (char (aref input row) col) #\#)))))

(day3-solver-loop +test+ 1) ;-> 2
(day3-solver-loop +test+ 3) ;-> 7
(day3-solver-loop +test+ 5) ;-> 3
(day3-solver-loop +test+ 7) ;-> 4

(day3-solver-loop +input+ 1) ;-> 84
(day3-solver-loop +input+ 3) ;-> 195
(day3-solver-loop +input+ 5) ;-> 70
(day3-solver-loop +input+ 7) ;-> 70
(day3-solver-loop +input+ 1 2) ;-> 47

; 3772314000
(reduce #'* (mapcar (lambda (x y) (day3-solver-loop +input+ x y)) '(1 3 5 7 1) '(1 1 1 1 2)))
(reduce #'* (mapcar (lambda (x y) (day3-solver-iterate +input+ x y)) '(1 3 5 7 1) '(1 1 1 1 2)))
