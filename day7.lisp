(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows :rove))

(defpackage :day7
  (:use :cl :binding-arrows :iterate)
  (:local-nicknames (:a :alexandria)))
(in-package :day7)

(defun create-bags (bag-str)
  (let ((bag-list '()))
    (ppcre:do-register-groups ((#'parse-integer num) bag-str)
        ("(\\d+) (\\w+ \\w+)" bag-str)
      (push (list num bag-str) bag-list))
    bag-list))

(defun read-input (filename)
  (iter
    (with bag-table = (make-hash-table :test #'equal))
    (for line in (uiop:read-file-lines filename))
    (for (first second) = (str:split " contain " line))
    (for key = (ppcre:scan-to-strings "^\\w+ \\w+" first))
    (setf (gethash key bag-table) (create-bags second))
    (finally (return bag-table))))

(defun dfs-p1 (index depth list search-term)
  (iter (for (count colour) in list)
    (when (equal colour search-term)
      (return-from dfs-p1 t))
    (collect (dfs-p1 index (1+ depth) (gethash colour index) search-term))))

(defun dfs-start-p1 (index search-term)
  (iter (for (ele children) in-hashtable index)
    (count (a:flatten (dfs-p1 index 1 children search-term)))))

(defun dfs-p2 (index depth list-term)
  (let ((seq (gethash list-term index)))
    (when (not seq)
      (return-from dfs-p2 0))
    (iter (for (count colour) in seq)
      (summing (+ count (* count (dfs-p2 index (1+ depth) colour))) into bag-count)
      (finally (return bag-count)))))

(defun day7-solver-p1 (filename)
  (dfs-start-p1 (read-input filename) "shiny gold"))

(defun day7-solver-p2 (filename)
  (dfs-p2 (read-input filename) 0 "shiny gold"))

;(day7-solver-p1 "day7-input")
