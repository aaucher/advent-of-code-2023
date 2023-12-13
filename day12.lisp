(defpackage #:advent2023.day12
  (:use :cl :day00))
(in-package #:advent2023.day12)

(defstruct row springs damaged)

(defun parse-spring (str)
  (loop for c across str collect c))

(defun parse-damaged (str)
  (mapcar #'parse-integer (str:split "," str)))

(defun parse-row (line)
  (let ((parts (str:split " " line)))
    (make-row :springs (parse-spring (first parts))
	      :damaged (parse-damaged (second parts)))))

(defun parse-field (lines)
  (mapcar #'parse-row lines))

(defun damagedp (c)
  (char= c #\#))

(defun unknownp (c)
  (char= c #\?))

(defun operationalp (c)
  (char= c #\.))

(defun breakp (c)
  (or (null c) (operationalp c) (unknownp c)))

(defun all-operational (springs)
  (every #'(lambda (s) (or (operationalp s) (unknownp s))) springs))

(defun possible-arrangements (row)
  (labels ((rec (to-match available damaged)
	     (cond
	       ;; successful search
	       ((and (null damaged) (all-operational available)) 1)
	       ;; unsuccessful seatch
	       ((null damaged) 0)
	       ;; skip operational
	       ((and (null to-match) (not (null available)) (operationalp (first available)))
		(rec '() (rest available) damaged))
	       ;; unknown is either operational or a damaged spring
	       ((and (null to-match) (not (null available)) (unknownp (first available)))
		(+ (rec '() (rest available) damaged)
		   (rec (list (first available)) (rest available) damaged)))
	       ;; first damaged count matches to-match
	       ((and (= (length to-match) (first damaged))
		     (breakp (first available)))
		(rec '() (rest available) (rest damaged)))
	       ;; no match, keep accumulating
	       ((and (not (null available))
		     (not (operationalp (first available)))
		     (< (length to-match) (first damaged)))
		(rec (cons (first available) to-match) (rest available) damaged))
	       ;; unsuccessful search
	       (t 0))))
    (rec '() (row-springs row) (row-damaged row))))

(defun solve-problem-1 (filepath)
  (reduce #'+ (mapcar #'possible-arrangements (parse-field (get-file filepath)))))

(solve-problem-1 #p"inputs/example12.txt") ;; 21
(solve-problem-1 #p"inputs/day12.txt") ;; 6871
