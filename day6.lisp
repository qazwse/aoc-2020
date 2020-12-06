(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :rove))

(defpackage :day6
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria)))
(in-package :day6)


(defun read-input (filename)
  (let ((nlnl (coerce '(#\Newline #\Newline) 'string))
        (nl   (coerce '(#\Newline) 'string)))
    (->> filename
      (uiop:read-file-string)
      (str:trim)
      (str:split nlnl)
      (mapcar #'(lambda (s) (str:replace-all " " "" s)))
      (mapcar #'(lambda (s) (str:split nl s)))
      (mapcar #'(lambda (g) (mapcar #'(lambda (s) (coerce s 'list)) g))))))

(defun day6-solver (filename)
  (->> filename
    (read-input)
    (print)
    (mapcar #'(lambda (g) (reduce #'intersection g)))
    (mapcar #'length)
    (reduce #'+)))

(day6-solver "day6-input")
(day6-solver "day6-test")
