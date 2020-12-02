;;;; Day 2 - Password Philosophy
;;;; Part 1 - Find passwords that don't match criteria

(ql:quickload '(:cl-ppcre))

(defparameter *test-input* (uiop:read-file-lines "day2-test"))
(defparameter *problem-input* (uiop:read-file-lines "day2-input"))

(defun parse-char (s)
  (coerce s 'character))

(defun parse-input (input)
  (ppcre:register-groups-bind ((#'parse-integer n1 n2) (#'parse-char char) password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" input)
    (list :n1 n1 :n2 n2 :char char :password password)))

(defun valid-password? (criteria)
  (let* ((min (getf criteria :n1))
         (max (getf criteria :n2))
         (char (getf criteria :char))
         (pw  (getf criteria :password))
         (c   (count char pw)))
    (<= min c max)))

(defun xor (a b)
  (and (not (and a b))
       (or a b)))

(defun valid-password-2? (criteria)
  (let ((p1 (1- (getf criteria :n1)))
        (p2 (1- (getf criteria :n2)))
        (char (getf criteria :char))
        (pw (getf criteria :password)))
    (xor (equal char (char pw p1)) (equal char (char pw p2)))))


;; Part 1
;; 519
(count-if #'valid-password? (mapcar #'parse-input *problem-input*))
;; Part 2
;; 708
(count-if #'valid-password-2? (mapcar #'parse-input *problem-input*))
