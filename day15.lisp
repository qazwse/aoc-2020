(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day15
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day15)

(defun solver-rec (end memory turn curr)
  (when (= turn end)
    (return-from solver-rec curr))
  (let* ((seen (aref memory curr))
         (next  (if (not seen)
                    0
                    (- turn seen))))
    (setf (aref memory curr) turn)
    (solver-rec end memory (1+ turn) next)))

(defun solver-loop (end memory start first)
  (iter
    (for turn initially start then (1+ turn))
    (for curr initially first then next)
    (until (= turn end))
    (for seen = (aref memory curr))
    (for next = (if (not seen)
                    0
                    (- turn seen)))
    (setf (aref memory curr) turn)
    (finally (return curr))))

(defun init-memory (num-list)
  (let ((memory (make-array '(30000000) :initial-element nil)))
    (dotimes (turn (length num-list) memory)
      (setf (aref memory (pop num-list)) (1+ turn)))))

(defun day15-solver (input end fn)
  (let* ((memory (loop-memory input))
         (turn   (1+ (length input))))
    (format t "Solution: ~A~%" (funcall fn end memory turn 0))))

; Part1:
; (day15-solver '(1 3 2) 2020) -> 1
; (day15-solver '(2 1 3) 2020) -> 10
; (day15-solver '(1 2 3) 2020) -> 27
; (day15-solver '(2 3 1) 2020) -> 78
; (day15-solver '(3 2 1) 2020) -> 438
; (day15-solver '(3 1 2) 2020) -> 1836
; (day15-solver '(9 12 1 4 17 0 18) 2020) -> 610
; Part2:
; (day15-solver '(1 3 2) 30000000) -> 2578
; (time (day15-solver '(9 12 1 4 17 0 18) 30000000 #'solver-loop)) -> 1407
; (time (day15-solver '(9 12 1 4 17 0 18) 30000000 #'solver-rec)) -> 1407
