(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day9
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia)))

(in-package :day9)

(defun day9-solver (filename)
  (let ((raw-data (uiop:read-file-lines filename)))))

;(day9-solver "day9-test")
;(day9-solver "day9-input")
