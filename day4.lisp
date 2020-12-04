;;;; Day 4

(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows))
(use-package :iterate)
(use-package :binding-arrows)
(declaim (optimize (speed 0) (safety 0) (debug 3)))

(defparameter *passport-flags* '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun parse-input (input)
  (iter
    (until (not input))
    (collect
        (iter (for line = (pop input))
          (until (not line))
          (until (equal line ""))
          (accumulate line :by (lambda (x y) (uiop:strcat x " " y)))))))

(defun valid-passport (passport)
  (->> (mapcar #'(lambda (key) (str:contains? key passport)) *passport-flags*)
    (reduce #'(lambda (x y) (and x y)))))

(defun day4-solver (filename)
  (->> filename
    uiop:read-file-lines
    parse-input
    (mapcar #'str:trim)
    (count-if #'valid-passport)))


(day4-solver "day4-input")

(mapcar #'str:trim (day4-solver "day4-test"))

(str:concat "asdf" "asdf")

(reduce (lambda (x y) (and x y)) '(t nil t nil) :initial-value t)
