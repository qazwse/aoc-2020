;;;; Advent of Code - Day 1
;;;; Report Repair
(ql:quickload '(:alexandria))

(defparameter *day1-test* (mapcar #'parse-integer (uiop:read-file-lines "day1-test")))
(defparameter *day1-input* (mapcar #'parse-integer (uiop:read-file-lines "day1-input")))

(defun find-sum (list length sum)
  (alexandria:map-permutations #'(lambda (x) (when (= sum (reduce #'+ x)) (return-from find-sum x))) list :length length))

(find-sum *day1-test* 2 2020)
(find-sum *day1-test* 3 2020)
(find-sum *day1-input* 2 2020)
(find-sum *day1-input* 3 2020)
