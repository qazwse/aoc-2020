(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day13
  (:use :cl :binding-arrows :series)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day13)


(defun parse-data (filename)
  (as-> (uiop:read-file-line filename :at 1) raw
    (str:split "," raw)
    (mapcar #'(lambda (s i) (cons (parse-integer s :junk-allowed t) i))
            raw
            (a:iota (length raw)))
    (remove-if #'(lambda (e) (null (car e))) raw)))

(defun day13-solver (filename &optional (start 0))
  (->> filename
    (parse-data)
    (p2-solver start)
    (format t "Solved: ~A~%")))

;(day13-solver "day13-test")
;(day13-solver "day13-test2")
;(day13-solver "day13-input" 100000000000000)
;(car (first (parse-data "day13-input")))


(defun p2 (data &optional (start 1) (step 1))
  (when (not data)
    (return-from p2 start))
  ())

(collect
    (until-if (lambda (i) (= 0 (mod i 67)))
              (scan-range :from 1)))
