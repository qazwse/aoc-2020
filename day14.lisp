(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day14
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day14)

;; Turn on - use or, stored value
;; Turn off - use and, 2^36 - stored value

(defun make-masks (str)
  (iter
    (for char in-vector str)
    (for pos downfrom (1- (length str)))
    (case char
      (#\1 (collect (ash 1 pos) into masks))
      (#\0 (collect (- (ash 1 pos)) into masks))
      (#\X (collect (ash 1 pos) into floats)))
    (finally (return (list masks floats)))))

(defun apply-masks (num masks)
  (iter
    (for mask in masks)
    (if (> mask 0)
        (setf num (logior num mask))
        (setf num (logand num (+ (1- (ash 1 36)) mask)))) ; addition because the mask will be negative
    (finally (return num))))

(defun apply-floats (num floats)
  (when (not floats)
    (return-from apply-floats (list num)))
  (let* ((mask (first floats))
         (num-or  (logior num mask))
         (num-and (logand num (- (1- (ash 1 36)) mask))))
    (append
      (apply-floats num-or  (rest floats))
      (apply-floats num-and (rest floats)))))

(defun p1-solver (data)
  (iter (with memory = (make-hash-table))
    (with masks = '())
    (for line in data)
    (when (search "mask" line)
      (setf masks (first (make-masks (subseq line 7))))
      (next-iteration))
    (re:register-groups-bind ((#'parse-integer mem-loc val))
        ("mem\\[(\\d+)\\] = (\\d+)" line)
      (setf (gethash mem-loc memory) (apply-masks val masks)))
    (finally (return (reduce #'+ (a:hash-table-values memory))))))

(defun p2-solver (data)
  (iter (with memory = (make-hash-table))
    (with masks = '())
    (with floats = '())
    (for line in data)
    (when (search "mask" line)
      (let ((mf (make-masks (subseq line 7))))
        (setf masks (remove-if #'(lambda (x) (< x 0)) (first mf)))
        (setf floats (second mf)))
      (next-iteration))
    (re:register-groups-bind ((#'parse-integer mem-loc val))
        ("mem\\[(\\d+)\\] = (\\d+)" line)
      (let* ((masked-mem (apply-masks mem-loc masks))
             (floating-mem (apply-floats masked-mem floats)))
        (mapcar #'(lambda (mem) (setf (gethash mem memory) val)) floating-mem)))
    (finally (return (reduce #'+ (a:hash-table-values memory))))))

(defun day14-solver (filename)
  (let ((data (uiop:read-file-lines filename)))
    (format t "P1: ~A~%" (p1-solver data)) ; 13727901897109
    (format t "P2: ~A~%" (p2-solver data)))) ; 5579916171823

; Do not run part 2 on test #1
;(time (day14-solver "day14-input"))
