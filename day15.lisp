(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia :series))

(defpackage :day15
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia) (:re :ppcre)))

(in-package :day15)

(defun p1-solver (end memory turn curr)
  (when (= turn end)
    (return-from p1-solver curr))
  (let* ((seen (gethash curr memory 0))
         (next  (if (= seen 0)
                    0
                    (- turn seen))))
    (setf (gethash curr memory) turn)
    (p1-solver end memory (1+ turn) next)))

(defun init-memory (num-list)
  (let ((memory (make-hash-table :size 1000)))
    (dotimes (turn (length num-list) memory)
      (setf (gethash (pop num-list) memory) (1+ turn)))))

(defun day15-solver (input end)
  (let* ((memory (init-memory input))
         (turn   (1+ (hash-table-count memory))))
    (format t "Solution: ~A~%" (p1-solver end memory turn 0))))

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
; (day15-solver '(9 12 1 4 17 0 18) 30000000) -> 1407
