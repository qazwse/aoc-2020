(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :trivia))

(defpackage :day9
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria) (:t :trivia)))

(in-package :day9)

(defun read-n (stream n)
  (iter (repeat n)
    (for i = (read stream))
    (collect i at beginning)))

(defun n-sum (list length sum)
  (a:map-permutations
    #'(lambda (x) (when (= sum (reduce #'+ x)) (return-from n-sum t)))
    list :length length)
  nil)

(defun solve-p1 (filename preamble)
  (with-open-file (in filename)
    (iter
      (for raw-data initially (read-n in preamble) then (cons i raw-data))
      (for i = (read in nil))
      (while i)
      (unless invalid
        (progn
          (for cipher = (subseq raw-data 0 preamble))
          (when (not (n-sum cipher 2 i)) (collect i into invalid))))
      (finally (return (values (first invalid) (coerce (reverse raw-data) 'vector)))))))

(defun p2-answer (data)
  (iter (for i in-vector data)
    (maximize i into max)
    (minimize i into min)
    (finally (return (+ max min)))))

(defun solve-p2 (data invalid)
  (iter
    (with sums = (make-hash-table :size (length data)))
    (for n in-vector data with-index i)
    (sum n into sum)
    (for seen = (gethash (- sum invalid) sums))
    (when (or seen (= invalid sum))
      (finish))
    (setf (gethash sum sums) i)
    (finally (return (if seen
                        (p2-answer (subseq data (1+ seen) (1+ i)))
                        (p2-answer (subseq data 0 (1+ i))))))))


(defun day9-solver (filename preamble)
  (multiple-value-bind (invalid data) (solve-p1 filename preamble)
    (format t "Part 1: Invalid number: ~A~%" invalid)
    (format t "Part 2: Answer: ~A~%" (solve-p2 data invalid))))

;(day9-solver "day9-test" 5)
;(time (day9-solver "day9-input" 25))
; (declaim (optimize (speed 3) (safety 0) (debug 0)))
