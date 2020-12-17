(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day16
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day16)

(defun parse-ranges (ranges)
  "Returns an list of a-lists, given a range in the form '14-295 or 598-1002'"
  (iter (for range in (str:split "or" ranges))
    (destructuring-bind (begin end) (str:split "-" range)
      (collect (cons (parse-integer begin) (parse-integer end))))))

(defun parse-rules (rule-str)
  (iter (for rule in (re:split "\\n" rule-str))
    (destructuring-bind (rule-name ranges) (str:split ":" rule)
      (collect (cons rule-name (parse-ranges (str:trim ranges)))))))

(defun parse-my-ticket (mt-str)
  (->> mt-str
    (re:split "\\n")
    (second)
    (str:split ",")
    (mapcar #'parse-integer)))

(parse-my-ticket "your ticket:
71,127,181,179,113,109,79,151,97,107,53,193,73,83,191,101,89,149,103,197")

(defun parse-nearby-tickets (nb-str)
  (->> nb-str
    (re:split "\\n")
    (rest)
    (mapcar #'(lambda (s) (->> s
                            (str:split ",")
                            (mapcar #'parse-integer))))))


(defun parse-input (filename)
  (destructuring-bind (r-str my-str nb-str) (re:split "\\n\\n" (uiop:read-file-string filename))
    (list (parse-rules r-str)
          (parse-my-ticket my-str)
          (parse-nearby-tickets nb-str))))


(defun p1-solver (rules nearby-tickets)
  (iter top-level
    (for ticket in nearby-tickets)
    (iter rule-level
      (for rule in rules)
      ;(for rule-name = (first rule))
      (for criteria = (rest rule))
      ;(format t "Rule: ~A~%" rule-name)
      (iter
        (for (min . max) in criteria)
        ;(format t "Criteria: ~A <= n <= ~A~%" min max)
        (in rule-level
            (collect
                (remove-if-not #'(lambda (x) (<= min x max)) ticket)
              into valid-nums)))
        ;(format t "Meet criteria: ~A~%" valid-nums))
      (finally (in top-level (collect (set-difference ticket (a:flatten valid-nums))))))))

(defun rule-applier (rules)
  (lambda (ticket)
    (iter (for num in ticket)
      (for valid = (iter applier (for rule in rules)
                     ;; First element of rule is the name, so we can skip it
                     (iter
                       (for (min . max) in (rest rule))
                       (in applier (collect (<= min num max))))))
      ;; We could probably check earlier but fuck that fuck this problem
      (never (every #'null valid))
      (finally (return t)))))

(defun cnt (item seq)
  (count item seq))

(defun p2-solver (my-ticket valid-tickets rules)
  ;; We'll use locations to save the positions that we've solved
  (let ((locations (make-hash-table :size (length rules))))
    (iter
      (for ticket in valid-tickets)
      (iter (for num in ticket)
        (for i from 1)
        (iter (for rule in rules)
          (for key = (first rule))
          (iter rule-loc (for (min . max) in (rest rule))
            (when (<= min num max)
              (setf (gethash key locations) (cons i (gethash key locations)))
              (return-from rule-loc))))))
    (iter (for i from 1 to (length my-ticket))
      (iter (for (rule fit) in-hashtable locations)
        (collect (cons rule (cnt i fit)))
        (break)))))


(defun day16-solver (filename)
  (destructuring-bind (rules my-ticket nearby-tickets) (parse-input filename)
    (format t "Solution 1: ~A~%" (reduce #'+ (a:flatten (p1-solver rules nearby-tickets))))
    (let* ((meets-criteria (rule-applier rules))
           (valid-tickets (remove-if-not meets-criteria nearby-tickets)))
      (format t "Solution 2: ~A~%" (p2-solver my-ticket valid-tickets rules)))))

(day16-solver "day16-test")
(day16-solver "day16-test1")

(declaim (optimize (speed 0) (debug 3)))
