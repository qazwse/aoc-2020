(defpackage :day11
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day11)

(defun read-file (filename)
  (as-> (uiop:read-file-lines filename) d
    (mapcar #'(lambda (line)
                (map 'list #'(lambda (c) (case c (#\L 1) (otherwise 0))) line)) d)
    (make-array (list (length d) (length (first d))) :initial-contents d)))
