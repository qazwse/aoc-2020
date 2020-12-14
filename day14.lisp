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
    (unless (char= #\X char)
      (collecting
        (case char
          (#\1 (expt 2 pos))
          (#\0 (- (expt 2 pos))))))))

;(make-masks "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") ;(64 -2)

(defun apply-masks (num masks)
  (iter
    (for mask in masks)
    (if (> mask 0)
        (setf num (logior num mask))
        (setf num (logand num (+ (1- (expt 2 36)) mask))))
    (finally (return num))))

(defun p1-solver (data)
  (iter (with memory = (make-hash-table))
    (with masks = '())
    (for line in data)
    (print line)
    (when (search "mask" line)
      (setf masks (make-masks (subseq line 7)))
      (print masks)
      (next-iteration))
    (let ((mem-loc (read-from-string (subseq line (search "[" line)))))
      (print mem-loc)
      (setf (gethash mem-loc memory) (apply-masks val masks)))))

(defun day14-solver (filename)
  (->> (uiop:read-file-lines filename)
    (p1-solver)
    (format t "Solved: ~A~%")))

(day14-solver "day14-test")
