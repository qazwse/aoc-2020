;;;; Day 4

(ql:quickload '(:alexandria :cl-ppcre :iterate))
(use-package :iterate)
(declaim (optimize (speed 0) (safety 0) (debug 3)))


(defun parse-input (input)
  (iter
    (until (not input))
    (collect
        (iter (for line = (pop input))
          (until (not line))
          (until (equal line ""))
          (accumulate line :by (lambda (x y) (uiop:strcat x " " y)) :initial-value "")))))


(defun valid-passport (passport)
  (print passport)
  (let ((len (count #\  passport)))
    (cond
      ((= len 6) t)
      ((and (= len 5) (not (find "cid" passport))) t)
      (t nil))))

(defun day4-solver (filename)
  (let ((lines (uiop:read-file-lines filename)))
    (parse-input lines)))

(day4-solver "day4-input")
