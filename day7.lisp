(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :rove))

(defpackage :day7
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria)))
(in-package :day7)

(defun make-bag (bag-info))


(defun read-input (filename)
  (iter
    (with bag-table = (make-hash-table))
    (for line in (uiop:read-file-lines filename))
    (for (first second) = (str:split " contain " line))
    (for bag = (ppcre:scan-to-strings ""))

    (setf (gethash bag bag-table))))

(ppcre:scan )

|=
  (let ((bag-info (make-hash-table)))
    (->> filename
      uiop:read-file-line ; change to lines after testing
      (str:split " contain ")
      (setf (gethash))))
=|

(defun day7-solver (filename)
  (->> filename
    (read-input)))


;(day7-solver "day7-input")
;(day7-solver "day7-test")
