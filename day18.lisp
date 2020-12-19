(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day18
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day18)

(defun weird-math (str &optional (start 0))
  (iter
    (with str = (str:substring start t str))
    (with skip = -1)
    (with fn = #'+)
    (for char in-string str with-index i)
    (when (<= i skip)
      (next-iteration))
    (when (not char)
      (finish))
    (case char
      (#\(
       (multiple-value-bind (res end) (weird-math str (1+ i))
         (reducing res by fn into result)
         (setf skip (+ i end 1))))
      (#\) (finish))
      (#\Space (next-iteration))
      (#\+ (setf fn #'+))
      (#\* (setf fn #'*))
      (otherwise (reducing (digit-char-p char) by fn into result)))
    (finally
     (return-from weird-math (values result i)))))

(defun day18-solver (filename)
  (->> (uiop:read-file-lines filename)
       (mapcar #'weird-math)
       (reduce #'+)))

(print (day18-solver "day18-input"))
