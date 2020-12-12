(defpackage :day12
  (:use :cl :binding-arrows :iterate :trivia)
  (:local-nicknames (:a :alexandria) (:re :ppcre)))

(use-package :trivia.ppcre)

(in-package :day12)

(defun get-new-heading (cur dir rot)
  (let* ((headings #(#\N #\E #\S #\W))
         (cur-abs   (position cur headings)))
    (if (char= #\R dir)
      (aref headings (mod (+ cur-abs (/ rot 90))  4))
      (aref headings (mod (+ cur-abs (/ rot -90)) 4)))))

(defun p1-solver (data)
  (let ((heading #\E)
        (v-dist 0)
        (h-dist 0)
        (new '()))
    (dolist (instr data new)
      (destructuring-bind (code val) instr
        (case code
          ((#\R #\L) (setf heading (get-new-heading heading code val)))
          (#\F (push (list heading val) new))
          (otherwise (push (list code val) new)))))
    (dolist (instr (reverse new))
      (destructuring-bind (code val) instr
        (case code
          (#\N (setf v-dist (+ v-dist val)))
          (#\E (setf h-dist (+ h-dist val)))
          (#\S (setf v-dist (- v-dist val)))
          (#\W (setf h-dist (- h-dist val))))))
    (+ (abs v-dist) (abs h-dist))))

(defun rotate-x (dir rot x)
  (let* ((headings (list #'+ #'+ #'- #'-)))
    (if (char= #\R dir)
        (apply (nth (1+ (mod (/ rot 90) 4)) headings) x)
        (apply (nth (1+ (mod (/ rot -90) 4)) headings) x))))

(defun rotate-xy (pt org dir theta)
  (let* ((theta (if (char= dir #\L) (- theta) theta))
         (rad (* pi (/ theta 180.0)))
         (x (car pt))
         (y (cdr pt))
         (ox (car org))
         (oy (cdr org))
         (ad-x (- x ox))
         (ad-y (- y oy))
         (cos-rad (cos rad))
         (sin-rad (sin rad)))
    (cons
     (round (+ ox
               (+ (* cos-rad ad-x)
                  (* sin-rad ad-y))))
     (round (+ oy
               (+ (* (- sin-rad) ad-x)
                  (* cos-rad ad-y)))))))

(defun p2-solver (data)
  (let ((v-dist 0)
        (h-dist 0)
        (wp (cons 10 1)))
    (dolist (instr data)
      (destructuring-bind (code val) instr
        (case code
          ((#\R #\L) (setf wp (rotate-xy wp (cons 0 0) code val)))
          (#\F (setf h-dist (+ h-dist (* (car wp) val)))
               (setf v-dist (+ v-dist (* (cdr wp) val))))
          (#\N (setf (cdr wp) (+ (cdr wp) val)))
          (#\E (setf (car wp) (+ (car wp) val)))
          (#\S (setf (cdr wp) (- (cdr wp) val)))
          (#\W (setf (car wp) (- (car wp) val))))))
    (+ (abs v-dist) (abs h-dist))))

(defun parse-data (data)
  (->> data
    (mapcar #'(lambda (s) (list (aref s 0) (parse-integer (subseq s 1)))))))

(defun day12-solver (filename)
  (let* ((data (uiop:read-file-lines filename))
         (data (parse-data data)))
    (format t "P1: ~A~%" (p1-solver data))
    (format t "P2: ~A~%" (p2-solver data))))

(day12-solver "day12-input")

(declaim (optimize (speed 0) (safety 0) (debug 3)))
