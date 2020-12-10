(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day10
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day10)

(defun p1-solver (data)
  (iter
    (for n in data)
    (for prev-n previous n initially 0)
    (for diff = (- n prev-n))
    (case diff
      (1 (counting n into ones))
      (2 (counting n into twos))
      (3 (counting n into threes)))
    ;(format t "~A ~A ~A~%" n prev-n diff)
    (finally (return (list ones twos threes)))))


(defun p2-solver (data)
  (iter (with dp = (make-hash-table))
    (initially (setf (gethash 0 dp) 1))
    (for i in data)
    (for first  = (gethash (- i 1) dp 0))
    (for second = (gethash (- i 2) dp 0))
    (for third  = (gethash (- i 3) dp 0))
    (for sum    = (+ first second third))
    (setf (gethash i dp) sum)
    (finally (return (gethash (a:lastcar data) dp)))))

(defun day10-solver (filename)
  (let* ((raw-data (uiop:read-file-lines filename))
         (int-data (mapcar #'parse-integer raw-data))
         (sorted-data (sort int-data #'<))
         (sorted-data (append sorted-data (list (+ 3 (a:lastcar sorted-data))))))
    (format t "Part 1: ~A~%" (p1-solver sorted-data))
    (format t "Part 2: ~A~%" (p2-solver sorted-data))))

;(day10-solver "day10-input") - 66,28
;(day10-solver "day10-test")
;(day10-solver "day10-test2")
