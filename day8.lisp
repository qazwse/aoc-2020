(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :rove))

(defpackage :day8
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria)))
(in-package :day8)

(defparameter *debug* t)

(defun symbolize (s)
  "Turn a string into a keyword"
  (intern (string-upcase s) (find-package :keyword)))

(defun counter ()
  (let ((counter -1))
    (lambda ()
      (incf counter))))

(defun create-op (str key-fn)
  (let ((split (str:split " " str)))
    (list (symbolize (first split)) (parse-integer (second split)) (funcall key-fn))))

(defun get-offset (op val)
  (case op
    (:JMP val)
    (otherwise 1)))

(defun run-bc (boot-code)
  (let* ((bc-len  (length boot-code))
         (visited (make-hash-table :size bc-len)))
    (iter
      (for ptr initially 0 then (+ ptr (get-offset op value)))
      (when (= ptr bc-len)
        (finish))
      (when (not (<= 0 ptr bc-len))
        (leave (list :out-of-bounds ptr accumulator)))
      (for (op value key) = (aref boot-code ptr))
      (when (> (incf (gethash key visited 0)) 1)
        (leave (list :infinite-loop accumulator))) ; We can return the current key, it causes an infinite loop
      (when (eq :acc op)
        (summing value into accumulator))
      (finally (return (list :complete accumulator))))))

(defun bad-instruction-loop (boot-code)
  (let ((old nil))
    (iter (for op in-vector boot-code :with-index i)
      (case (first op)
        (:JMP (progn (setf old op) (setf (aref boot-code i) (cons :nop (rest op)))))
        (:NOP (progn (setf old op) (setf (aref boot-code i) (cons :jmp (rest op)))))
        (:ACC (next-iteration)))
      (for run = (run-bc boot-code))
      (case (first run)
        ((:infinite-loop :out-of-bounds) (setf (aref boot-code i) old))
        (:complete (finish)))
      (finally (return run)))))

(defun day8-solver (filename)
  (let* ((raw-data  (uiop:read-file-lines filename))
         (key-fn    (counter))
         (boot-code (map 'vector #'(lambda (s) (create-op s key-fn)) raw-data)))
    (bad-instruction-loop boot-code)))

(day8-solver "day8-test")
(day8-solver "day8-input")
;(time (day8-solver "day8-big-large"))
