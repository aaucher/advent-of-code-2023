(defpackage #:advent2023.day01
  (:use :cl))
(in-package #:advent2023.day01)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while (not (str:emptyp line))
          collect line)))

(defun first-digit (str)
  (loop for c across str do
    (let ((d (digit-char-p c)))
      (when d (return d)))))

(defun last-digit (str)
  (first-digit (reverse str)))

(defun extract-2-digits-number (str)
  (let ((f (first-digit str))
	(l (last-digit str)))
    (+ (* f 10) l)))

(defun solve-problem1 (filepath)
  (reduce #'+ (mapcar #'extract-2-digits-number (get-file filepath))))

(solve-problem1 #P"inputs/day01.txt")

(defparameter *spelled-out-digits*
  '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defun starts-with-digits? (str &key reverse)
  (loop for digit in (if reverse
			 (mapcar #'reverse *spelled-out-digits*)
			 *spelled-out-digits*)
	for y from 1
	when (str:starts-with? digit str) do
	  (return y)))

(defun first-digit-or-spelled-out-digit (str &key reverse)
  (let ((input (if reverse (reverse str) str)))
    (loop for c across input
	  for y from 0 do
	    (let ((d (digit-char-p c))
		  (w (starts-with-digits? (subseq input y) :reverse reverse)))
	      (when d (return d))
	      (when w (return w))))))

(defun extract-2-digits-or-spelled-out-digit-number (str)
  (let ((f (first-digit-or-spelled-out-digit str))
	(l (first-digit-or-spelled-out-digit str :reverse t)))
    (+ (* f 10) l)))

(defun solve-problem2 (filepath)
  (reduce #'+ (mapcar #'extract-2-digits-or-spelled-out-digit-number (get-file filepath))))

(solve-problem2 #P"inputs/example01-2.txt")
