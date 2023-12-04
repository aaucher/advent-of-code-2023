(defpackage #:advent2023.day03
  (:use :cl :day00))
(in-package #:advent2023.day03)

(defstruct schematic types numbers)

(defun parse-number (current-number)
  (parse-integer (map 'string #'identity (reverse current-number))))

(defun record-number (current-number numbers x y)
  (let ((num (parse-number current-number)))
    (loop for p from (length current-number) downto 1 do
      (setf (gethash (cons (- x p) y) numbers) num))))

(defun parse-schematic (lines)
  (let ((types (make-hash-table :test #'equal))
	(numbers (make-hash-table :test #'equal))
	current-number)
    (loop for line in lines
	  for y from 0 do
	    (loop for c across line
		  for x from 0
		  with max-x = (1- (length line)) do
		    (cond ((digit-char-p c)
			   (setf (gethash (cons x y) types) :digit)
			   (push c current-number)
			   (when (and (= x max-x) current-number)
			     (record-number current-number numbers x y)
			     (setf current-number nil)))
			  ((char= c #\.)
			   (when current-number
			     (record-number current-number numbers x y)
			     (setf current-number nil)))
			  (t (setf (gethash (cons x y) types) (if (char= c #\*)
								  :*
								  :symbol))
			     (when current-number
			       (record-number current-number numbers x y)
			       (setf current-number nil))))))
    (make-schematic :types types :numbers numbers)))

(defun part-numbers-around (schematic x y)
  (let* ((numbers (schematic-numbers schematic))
	 (left (gethash (cons (1- x) y) numbers))
	 (right (gethash (cons (1+ x) y) numbers))
	 (up (gethash (cons x (1- y)) numbers))
	 (down (gethash (cons x (1+ y)) numbers))
	 (upper-left (gethash (cons (1- x) (1- y)) numbers))
	 (upper-right (gethash (cons (1+ x) (1- y)) numbers))
	 (lower-left (gethash (cons (1- x) (1+ y)) numbers))
	 (lower-right (gethash (cons (1+ x) (1+ y)) numbers))
	 part-numbers)
    (when left (push left part-numbers))
    (when right (push right part-numbers))
    (if up
	(push up part-numbers)
	(progn (when upper-left (push upper-left part-numbers))
	       (when upper-right (push upper-right part-numbers))))
    (if down
	(push down part-numbers)
	(progn (when lower-left (push lower-left part-numbers))
	       (when lower-right (push lower-right part-numbers))))
    part-numbers))

(defun get-part-numbers (schematic)
  (loop for k being the hash-keys in (schematic-types schematic) using (hash-value v)
	for x = (car k)
	for y = (cdr k)
	when (or (eq v :symbol) (eq v :*))
	  nconcing (part-numbers-around schematic x y)))

(defun solve-problem-1 (filepath)
  (reduce #'+ (get-part-numbers (parse-schematic (get-file filepath)))))

(solve-problem-1 #P"inputs/example03.txt") ;; 4361
(solve-problem-1 #P"inputs/day03.txt") ;; 536576

(defun gear-ratio (schematic x y)
  (let ((part-numbers-around (part-numbers-around schematic x y)))
    (when (= 2 (length part-numbers-around))
      (reduce #'* part-numbers-around))))

(defun get-gear-ratios (schematic)
  (remove-if #'null
	     (loop for k being the hash-keys in (schematic-types schematic) using (hash-value v)
		   for x = (car k)
		   for y = (cdr k)
		   when (eq v :*)
		     collect (gear-ratio schematic x y))))

(defun solve-problem-2 (filepath)
  (reduce #'+ (get-gear-ratios (parse-schematic (get-file filepath)))))

(solve-problem-2 #P"inputs/example03.txt") ;; 467835
(solve-problem-2 #P"inputs/day03.txt") ;; 75741499
