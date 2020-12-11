(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day11
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day11)

(defun count-seats (floorplan)
  (destructuring-bind (height length) (array-dimensions floorplan)
    (iter outer
      (for i from 0 below height)
      (iter (for j from 0 below length)
        (in outer (count (eql :taken (aref floorplan i j))))))))

(defun count-neighbours (floorplan y x height length)
  (iter outer
    (for i from (1- y) to (1+ y))
    (iter
      (for j from (1- x) to (1+ x))
      (when (or (< i 0) (>= i height)
                (< j 0) (>= j length)
                (and (= i y) (= j x)))
        (next-iteration))
      (in outer (count (eql :taken (aref floorplan i j)))))))

(defun update-floorplan (floorplan)
  (destructuring-bind (height length) (array-dimensions floorplan)
    (iter (with new-fp = (make-array (list height length) :initial-element :floor))
      (for i from 0 below height)
      (iter (for j from 0 below length)
        (case (aref floorplan i j)
          (:empty (if
                   (= 0 (count-neighbours floorplan i j height length)) (setf (aref new-fp i j) :taken)
                   (setf (aref new-fp i j) :empty)))
          (:taken (if
                   (>= (count-neighbours floorplan i j height length) 4) (setf (aref new-fp i j) :empty)
                   (setf (aref new-fp i j) :taken)))))
      (finally (return new-fp)))))


(defun p1-solver (floorplan)
  (iter (for old-fp initially floorplan then fp)
    (for fp = (update-floorplan old-fp))
    (until (equalp old-fp fp))
    (finally (return (count-seats fp)))))

(defun p2-solver (data)
  data)

(defun day11-solver (filename)
  (let* ((data (as-> (uiop:read-file-lines filename) d
                 (mapcar #'(lambda (line)
                             (map 'list #'(lambda (c) (case c (#\L :taken) (otherwise :floor))) line))
                         d)))
         (data-array (make-array (list (length data) (length (first data))) :initial-contents data)))
    (format t "Part 1: ~A~%" (p1-solver data-array))))
    ;(format t "Part 2: ~A~%" (p2-solver data))))

(day11-solver "day11-test")
(day11-solver "day11-input")

(declaim (optimize (speed 0) (debug 3)))
