;;;; Day 2 - Password Philosophy
;;;; Part 1 - Find passwords that don't match criteria

(ql:quickload '(:cl-ppcre))
;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

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

(defun faster-test (input)
  (let ((count1 0)
        (count2 0))
    (dolist (c input (values count1 count2))
       (ppcre:register-groups-bind ((#'parse-integer p1 p2) (#'parse-char char) pw)
           (*regex* c)
         (when (<= p1 (count char pw) p2)
           (setf count1 (1+ count1)))
         (when (xor (equal char (char pw (1- p1))) (equal char (char pw (1- p2))))
           (setf count2 (1+ count2)))))))

(time (faster-test *big-input*))


;; Part 1
;; 519
(count-if #'valid-password? (mapcar #'parse-input *problem-input*))
;; Part 2
;; 708
(time (count-if #'valid-password-2? (mapcar #'parse-input *big-input*)))

;; P1-Big - 134116
;; P2-Big - 201017
