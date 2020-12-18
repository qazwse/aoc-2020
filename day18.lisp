(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day18
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day18)

(defun weird-math (str)
  (->> (replace " " str)))


(defun day18-solver (filename)
  (->> (uiop:read-file-lines filename)
       (mapcar #'weird-math)
       (reduce #'+)))
