(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day12
  (:use :cl :binding-arrows :series :trivia)
  (:local-nicknames (:a :alexandria) (:re :ppcre)))

(in-package :day12)

(defun get-new-heading (cur dir rot)
  (let* ((headings #(#\N #\E #\S #\W))
         (cur-abs   (position cur headings)))
    (if (char= #\R dir)
      (aref headings (mod (+ cur-abs (/ rot 90))  4))
      (aref headings (mod (+ cur-abs (/ rot -90)) 4)))))

(defun p1-solver (filename)
  (let ((heading #\E)
        (dist (cons 0 0))
        (new '()))
    (iterate ((instr (scan-file filename #'read-line)))
     (let ((code (aref instr 0))
           (val (parse-integer (subseq instr 1))))
        (case code
          ((#\R #\L) (setf heading (get-new-heading heading code val)))
          (#\F (push (list heading val) new))
          (#\N (setf (cdr dist) (+ (cdr dist) val)))
          (#\E (setf (car dist) (+ (car dist) val)))
          (#\S (setf (cdr dist) (- (cdr dist) val)))
          (#\W (setf (car dist) (- (car dist) val))))))
    (dolist (instr new)
      (destructuring-bind (code val) instr
        (case code
          (#\N (setf (cdr dist) (+ (cdr dist) val)))
          (#\E (setf (car dist) (+ (car dist) val)))
          (#\S (setf (cdr dist) (- (cdr dist) val)))
          (#\W (setf (car dist) (- (car dist) val))))))
    (+ (abs (car dist)) (abs (cdr dist)))))

(defun rotate-xy (pt theta)
  (let* ((rad (* pi (/ theta 180.0)))
         (x (car pt))
         (y (cdr pt))
         (cos-rad (cos rad))
         (sin-rad (sin rad)))
    (cons
     (round (+ (* cos-rad x)
               (* sin-rad y)))
     (round (+ (* (- sin-rad) x)
               (* cos-rad y))))))

(defun rotate-xy-l (pt theta))


(defun p2-solver (filename)
  (let ((dist (cons 0 0))
        (wp   (cons 10 1)))
    (iterate ((instr (scan-file filename #'read-line)))
      (let ((code (aref instr 0))
            (val (parse-integer (subseq instr 1))))
        (case code
          (#\R (setf wp (rotate-xy wp val)))
          (#\L (setf wp (rotate-xy wp (- val))))
          (#\F (setf (car dist) (+ (car dist) (* (car wp) val)))
               (setf (cdr dist) (+ (cdr dist) (* (cdr wp) val))))
          (#\N (setf (cdr wp) (+ (cdr wp) val)))
          (#\E (setf (car wp) (+ (car wp) val)))
          (#\S (setf (cdr wp) (- (cdr wp) val)))
          (#\W (setf (car wp) (- (car wp) val))))))
    (+ (abs (car dist)) (abs (cdr dist)))))

(defun day12-solver (filename)
  (format t "P1: ~A~%" (p1-solver filename))
  (format t "P2: ~A~%" (p2-solver filename)))

;(time (day12-solver "day12-bb1"))

(declaim (optimize (speed 3) (safety 0) (debug 0)))
