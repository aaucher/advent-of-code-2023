(defpackage #:advent2023.day09
  (:use :cl :day00))
(in-package #:advent2023.day09)

(defun parse-history (line)
  (mapcar #'parse-integer (str:words line)))

(defun parse-report (lines)
  (mapcar #'parse-history lines))

(defun diff-history (history)
  (let ((stack (list history)))
    (loop for top = (car stack)
	  while (not (every #'(lambda (x) (= x 0)) top))
	  do (push (loop for i from 1 below (length top)
			 collect (- (elt top i) (elt top (1- i))))
		   stack))
    stack))

(defun extrapolate-forward (history)
  (reduce #'+ (mapcan #'last (diff-history history))))

(defun solve-problem-1 (filepath)
  (reduce #'+ (mapcar #'extrapolate-forward (parse-report (get-file filepath)))))

(solve-problem-1 #p"inputs/example09.txt") ;; 114
(solve-problem-1 #p"inputs/day09.txt") ;; 1806615041

(defun extrapolate-backward (history)
  (reduce #'(lambda (prev curr) (- curr prev))
	  (mapcar #'first (diff-history history))
	  :initial-value 0))

(defun solve-problem-2 (filepath)
  (reduce #'+ (mapcar #'extrapolate-backward (parse-report (get-file filepath)))))

(solve-problem-2 #p"inputs/example09.txt") ;; 2
(solve-problem-2 #p"inputs/day09.txt") ;; 1211
