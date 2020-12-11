(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day11
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day11)

(defun day11-solver (filename jump)
  (let* ((data (as-> (uiop:read-file-lines filename) d
                (mapcar #'parse-integer d)
                (sort d #'<)
                (append d (list (+ 3 (a:lastcar d)))))))
    (format t "Part 1: ~A~%" (p1-solver-new data jump))
    (format t "Part 2: ~A~%" (p2-solver data jump))))
