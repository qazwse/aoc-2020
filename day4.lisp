;;;; Day 4

(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows))
(use-package :iterate)
(use-package :binding-arrows)
(declaim (optimize (speed 0) (safety 0) (debug 3)))

(defconstant +passport-flags+ '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun parse-input (input)
  (iter
    (until (not input))
    (collect
        (iter (for line = (pop input))
          (until (not line))
          (until (equal line ""))
          (accumulate line :by (lambda (x y) (uiop:strcat x " " y)))))))

(defun byr (year)
  (<= 1920 (parse-integer year) 2002))

(defun iyr (year)
  (<= 2010 (parse-integer year) 2020))

(defun eyr (year)
  (<= 2020 (parse-integer year) 2030))

(defun hgt (height)
  (let ((cm (search "cm" height))
        (in (search "in" height)))
    (cond (cm (<= 150 (parse-integer (subseq height 0 cm)) 193))
          (in (<= 59  (parse-integer (subseq height 0 in))  76))
          (t nil))))

(defun hcl (colour)
  (when (ppcre:scan "^\\#[0-9a-fA-F]{6}$" colour)
    t))

(defun ecl (colour)
  (when (search colour "amb blu brn gry grn hzl oth")
    t))

(defun pid (pid)
  (when (ppcre:scan "^\\d{9}$" pid)
    t))

(defun cid (cid)
  t)

(defun get-function (fn-name)
  (symbol-function (find-symbol (string-upcase fn-name))))

(defun valid-information? (passport)
  (iter
    (for flag in (str:split " " passport))
    (for split = (str:split ":" flag))
    (for res = (funcall (get-function (first split)) (second split)))
    (collect res into truths)
    (finally (return (reduce #'(lambda (x y) (and x y)) truths)))))

(defun has-fields? (passport)
  (->> (mapcar #'(lambda (key) (str:contains? key passport)) +passport-flags+)
    (reduce #'(lambda (x y) (and x y)))))

(defun valid-passport? (passport)
  (and
   (has-fields? passport)
   (valid-information? passport)))

(defun error-logger (passport)
  (when (valid-passport? passport)
    (print passport)))

(defun day4-solver (filename)
  (->> filename
    uiop:read-file-lines
    parse-input
    (mapcar #'str:trim)
    (count-if #'valid-passport?)))

(day4-solver "day4-input")
