(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :rove))

(defpackage :day5
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria)
                    (:t :rove)))
(in-package :day5)

(defun to-binary (str)
  (-<> (str:trim str)
    (map 'string (lambda (_) (case _ ((#\B #\R) #\1) (t #\0))) <>)
    (parse-integer <> :radix 2)))

(defun find-seat (min max sum)
  (let* ((range  (1+ (- max min)))
         (rsum   (/ (* range (+ max min)) 2)))
    (- rsum sum)))

(defun multi-reducer (fns)
  (lambda (vals num)
    (if (not vals)
        (make-list (length fns) :initial-element num)
        (mapcar (lambda (fn val) (funcall fn val num)) fns vals))))

(defun day5-solver-fn (filename)
  (as-> filename var
    (uiop:read-file-lines var)
    (mapcar #'to-binary var)
    (reduce (multi-reducer (list #'min #'max #'+)) var :initial-value nil)
    (destructuring-bind (min max sum) var (find-seat min max sum))))

(day5-solver-fn "day5-input")

(defun day5-solver (filename)
  (iter (for line in (uiop:read-file-lines filename))
    (for binary = (to-binary line))
    (minimize binary into min)
    (maximize binary into max)
    (summing  binary into sum)
    (finally (return (values min max sum (find-seat min max sum))))))

(day5-solver "day5-input")


(t:testing "to binary"
  (t:ok (= (to-binary "BFFFBBFRRR") #2r1000110111))
  (t:ok (= (to-binary "FFFBBBFRRR") #2r0001110111))
  (t:ok (= (to-binary "BBFFBBFRLL") #2r1100110100))))
