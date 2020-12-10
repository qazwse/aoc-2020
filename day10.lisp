(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day10
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day10)

(defun p1-solver (data)
  (iter (for line in data)
    (print line)))

(defun day10-solver (filename)
  (let ((raw-data (uiop:read-file-lines filename)))
    (format t "Part 1: ~A~%" (p1-solver raw-data))))



;(day10-solver "day10-input")
;(day10-solver "day10-test")
