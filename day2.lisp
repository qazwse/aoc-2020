;;;; Day 2 - Password Philosophy
;;;; Part 1 - Find passwords that don't match criteria

(ql:quickload '(:alexandria :cl-ppcre))
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *test-input* (uiop:read-file-lines "day2-test"))
(defparameter *problem-input* (uiop:read-file-lines "day2-input"))

(defun parse-input (input)
  (ppcre:register-groups-bind (min max char password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" input)
    (list :min (parse-integer min) :max (parse-integer max) :char (coerce char 'character) :password password)))

(defun valid-password? (criteria)
  (let* ((min (getf criteria :min))
         (max (getf criteria :max))
         (char (getf criteria :char))
         (pw  (getf criteria :password))
         (c   (count char pw)))
    (and (>= c min) (<= c max))))


;; Part 1
;; 519
(length (remove-if-not #'valid-password? (mapcar #'parse-input *problem-input*)))
