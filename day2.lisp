;;;; Day 2 - Password Philosophy
;;;; Part 1 - Find passwords that don't match criteria

(ql:quickload '(:cl-ppcre))

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

(defun xor (pred-a pred-b)
  (if (and pred-a pred-b)
      nil
      (or pred-a pred-b)))

(defun valid-password-2? (criteria)
  (let ((p1 (1- (getf criteria :min)))
        (p2 (1- (getf criteria :max)))
        (char (getf criteria :char))
        (pw (getf criteria :password)))
    (xor (equal char (char pw p1)) (equal char (char pw p2)))))


;; Part 1
;; 519
(length (remove-if-not #'valid-password? (mapcar #'parse-input *problem-input*)))
;; Part 2
;; 708
(length (remove-if-not #'valid-password-2? (mapcar #'parse-input *problem-input*)))
