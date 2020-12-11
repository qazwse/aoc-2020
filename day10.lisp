(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day10
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day10)

(defun p1-solver (data jump)
  (iter (with result = (make-array (1+ jump) :initial-element 0))
    (for n in data)
    (for prev-n previous n initially 0)
    (for diff = (- n prev-n))
    (incf (aref result diff))
    (finally
     (return (* (aref result 1) (aref result 3))))))

(defun p2-solver (data window &optional (curr '(0)) (memos (make-hash-table :size (a:lastcar data) :test #'eq)))
  (when (not data)
    (return-from p2-solver 1))
  (when (gethash (first curr) memos)
    (return-from p2-solver (gethash (first curr) memos)))
  (iter
    (for n = (pop data))
    (while n)
    (while (<= (- n (first curr)) window))
    (summing (p2-solver data window (cons n curr) memos) into c)
    (finally
     (setf (gethash (first curr) memos) c)
     (return c))))

(defun day10-solver (filename jump)
  (let* ((raw-data (uiop:read-file-lines filename))
         (int-data (mapcar #'parse-integer raw-data))
         (sorted-data (sort int-data #'<))
         (sorted-data (append sorted-data (list (+ 3 (a:lastcar sorted-data))))))
    (format t "Part 1: ~A~%" (p1-solver-new sorted-data jump))
    (format t "Part 2: ~A~%" (p2-solver sorted-data jump))))

;(time (day10-solver "day10-input" 3)) - 66,28
;(day10-solver "day10-test" 3)
;(day10-solver "day10-test2" 3)
