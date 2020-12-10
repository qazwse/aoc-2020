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
    (format t "~A ~A ~A~%" n prev-n diff)
    (finally (return (list ones twos (1+ threes))))))

(defun day10-solver (filename)
  (let* ((raw-data (uiop:read-file-lines filename))
         (int-data (mapcar #'parse-integer raw-data))
         (sorted-data (sort int-data #'<)))
    (format t "Part 1: ~A~%" (p1-solver sorted-data))))



;(day10-solver "day10-input")
;(day10-solver "day10-test")
;(day10-solver "day10-test2")
