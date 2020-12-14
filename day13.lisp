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

(defun day13-solver (filename)
  (->> filename
    (parse-data)
    (p2-solver)
    (format t "Solved: ~A~%")))

;(day13-solver "day13-input")
;(day13-solver "day13-test2")


(defun p2-solver (data &optional (start 1) (step 1))
  (when (not data)
    (return-from p2-solver start))
  (format t "Finding ~A stepping ~A starting at ~A~%" (car (first data)) step start)
  (let* ((n (- (car (first data))))
         (a (- (cdr (first data))))
         (step (abs step))
         (start (collect-first
                 (choose-if (lambda (i) (= a (mod i n)))
                            (scan-range :from start :by step)))))
    (p2-solver (rest data) start (* step n))))

(defun p2-solver-faster)

(declaim (optimize (speed 0) (safety 0) (debug 3)))
