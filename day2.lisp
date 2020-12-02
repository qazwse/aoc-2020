;;;; Day 2 - Password Philosophy
;;;; Part 1 - Find passwords that don't match criteria

(ql:quickload '(:cl-ppcre))
;; (declaim (optimize (speed 0) (safety 0) (debug 3)))

(defparameter *test-input* (uiop:read-file-lines "day2-test"))
(defparameter *problem-input* (uiop:read-file-lines "day2-input"))
(defparameter *big-input* (uiop:read-file-lines "day2-big"))

(defvar *regex* (ppcre:create-scanner "(\\d+)-(\\d+) (\\w): (\\w+)"))

(defun parse-char (s)
  (coerce s 'character))

(defun parse-input (input)
  (ppcre:register-groups-bind ((#'parse-integer n1 n2) (#'parse-char char) password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" input)
    (list n1 n2 char password)))

(defun valid-password? (criteria)
  (destructuring-bind (min max char pw) criteria
    (<= min (count char pw) max)))

(defun xor (a b)
  (and (not (and a b))
       (or a b)))

(defun valid-password-2? (criteria)
  (destructuring-bind (p1 p2 char pw) criteria
    (xor (equal char (char pw (1- p1))) (equal char (char pw (1- p2))))))



;; Part 1
;; 519
(count-if #'valid-password? (mapcar #'parse-input *problem-input*))
;; Part 2
;; 708
(count-if #'valid-password-2? (mapcar #'parse-input *problem-input*))
