(defpackage #:advent2023.day04
  (:use :cl))
(in-package #:advent2023.day04)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while (not (str:emptyp line))
          collect line)))

(defstruct card number winning-numbers numbers)

(defun parse-card-number (str)
  (parse-integer (car (last (str:split " " str)))))

(defun parse-numbers (str)
  (mapcar #'parse-integer (remove-if #'str:emptyp (str:split " " str))))

(defun parse-card (line)
  (let* ((card (str:split ": " line))
	 (content (str:split " | " (cadr card))))
    (make-card :number (parse-card-number (car card))
	       :winning-numbers (parse-numbers (car content))
	       :numbers (parse-numbers (cadr content)))))

(defun matches (winning-numbers numbers)
  (intersection winning-numbers numbers))

(defun card-value (card)
  (let ((matches (matches (card-winning-numbers card) (card-numbers card))))
    (if (null matches)
	0
	(expt 2 (1- (length matches))))))

(defun solve-problem-1 (filepath)
  (reduce #'+ (mapcar (lambda (line)
			(card-value (parse-card line)))
		      (get-file filepath))))

(solve-problem-1 #P"inputs/example04.txt")
(solve-problem-1 #P"inputs/day04.txt")

(defun copies (cards)
  (let ((copies (make-array (length cards) :initial-element 1)))
    (dolist (card cards)
      (let ((card-id (card-number card))
	    (n-matches (length (matches (card-winning-numbers card) (card-numbers card)))))
	(loop for n from card-id below (+ card-id n-matches) do
	  (setf (aref copies n) (+ (aref copies n) (aref copies (1- card-id)))))))
    copies))

(defun solve-problem-2 (filepath)
  (reduce #'+ (copies (mapcar #'parse-card (get-file filepath)))))

(solve-problem-2 #P"inputs/example04.txt")
(solve-problem-2 #P"inputs/day04.txt")
