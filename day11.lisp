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

(defun count-neighbours-radial (floorplan y x height length)
  (iter outer
    (for i from (1- y) to (1+ y))
    (iter
      (for j from (1- x) to (1+ x))
      (when (or (< i 0) (>= i height)
                (< j 0) (>= j length)
                (and (= i y) (= j x)))
        (next-iteration))
      (in outer (count
                 (iter
                   (with y-dir = (- i y))
                   (with x-dir = (- j x))
                   (for i2 initially i then (+ i2 y-dir))
                   (for j2 initially j then (+ j2 x-dir))
                   (until (or (< i2 0) (>= i2 height)
                              (< j2 0) (>= j2 length)))
                   (for seat = (aref floorplan i2 j2))
                   (when (eql :taken seat)
                     (leave t))
                   (when (eql :empty seat)
                     (leave nil))))))))

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

(defun update-floorplan (floorplan neighbour-check limit)
  (destructuring-bind (height length) (array-dimensions floorplan)
    (iter (with new-fp = (make-array (list height length) :initial-element :floor))
      (for i from 0 below height)
      (iter (for j from 0 below length)
        (case (aref floorplan i j)
          (:empty (if
                   (= 0 (funcall neighbour-check floorplan i j height length)) (setf (aref new-fp i j) :taken)
                   (setf (aref new-fp i j) :empty)))
          (:taken (if
                   (>= (funcall neighbour-check floorplan i j height length) limit) (setf (aref new-fp i j) :empty)
                   (setf (aref new-fp i j) :taken)))))
      (finally (return new-fp)))))

(defun solver (floorplan neighbour-check limit)
  (iter (for old-fp initially floorplan then fp)
    (for fp = (update-floorplan old-fp neighbour-check limit))
    (until (equalp old-fp fp))
    (finally (return (count-seats fp)))))

(defun day11-solver (filename)
  (let* ((data (as-> (uiop:read-file-lines filename) d
                 (mapcar #'(lambda (line)
                             (map 'list #'(lambda (c) (case c (#\L :taken) (otherwise :floor))) line))
                         d)))
         (data-array (make-array (list (length data) (length (first data))) :initial-contents data)))
    (format t "Part 1: ~A~%" (solver data-array #'count-neighbours 4))
    (format t "Part 2: ~A~%" (solver data-array #'count-neighbours-radial 5))))

(time (day11-solver "day11-big"))
