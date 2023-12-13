(defpackage #:advent2023.day13
  (:use :cl :day00))
(in-package #:advent2023.day13)

(defun parse-patterns (lines)
  (mapcar #'(lambda (pattern)
	      (mapcar #'(lambda (str) (loop for c across str collect c)) pattern))
	  (split-when #'str:emptyp lines)))

(defun reflectionp (list-a list-b)
  (every #'equal list-a (reverse list-b)))

(defun reflection-axis (pattern)
  (let ((len (length pattern)))
    (loop for width from (floor (/ len 2)) downto 1 do
      (loop for pos in (list width (- len width))
	    when (reflectionp (subseq pattern (- pos width) pos)
			      (subseq pattern pos (+ pos width)))
	      do (return-from reflection-axis (cons pos width))))))

(defun reflection (pattern)
  (let ((horizontal-axis (reflection-axis pattern))
	(vertical-axis (reflection-axis (rotate pattern))))
    (if (> (or (cdr horizontal-axis) 0) (or (cdr vertical-axis) 0))
	(cons :h horizontal-axis)
	(cons :v vertical-axis))))

(defun score (reflection-axis)
  (if (eq :v (first reflection-axis))
      (second reflection-axis)
      (* 100 (second reflection-axis))))

(defun solve-problem-1 (filepath)
  (reduce #'+ (mapcar #'(lambda (pattern)
			  (score (reflection pattern)))
		      (parse-patterns (get-file filepath)))))

(solve-problem-1 #p"inputs/example13.txt") ;; 405
(solve-problem-1 #p"inputs/day13.txt") ;; 34821
