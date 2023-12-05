(defpackage #:advent2023.day00
  (:use :cl)
  (:nicknames #:day00)
  (:export #:get-file
	   #:list-min
	   #:list-max
	   #:split-when
	   #:chunk
	   #:range
	   #:make-range
	   #:range-from
	   #:range-to
	   #:range-+
	   #:range-=
	   #:range-from-<
	   #:make-range-from-width
	   #:range-disjoint-p
	   #:range-intersection
	   #:range-union
	   #:range-difference
	   #:range-min
	   #:range-max))
(in-package #:advent2023.day00)

(defun get-file (filepath &key skip-empty-lines)
  "Read the content of the file at FILEPATH as a line of lines."
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil :eof)
	  while (not (eq line :eof))
          when (not (and skip-empty-lines (str:emptyp line)))
          collect line)))

(defun list-min (list)
  (apply #'min list))

(defun list-max (list)
  (apply #'max list))

(defun split-when (predicate list)
  (labels ((cut (l acc)
	     (if (null l)
		 (nreverse acc)
		 (let ((at (position-if predicate l)))
		   (if (not at)
		       (cut '() (cons l acc))
		       (cut (subseq l (1+ at)) (cons (subseq l 0 at) acc)))))))
    (cut list '())))

(defun chunk (list size)
  (labels ((cut (l acc)
	     (if (null l)
		 (nreverse acc)
		 (cut (subseq l size) (cons (subseq l 0 size) acc)))))
    (cut list '())))

(defstruct range from to) ;; inclusive of bounds

(defun range-+ (range scalar)
  (make-range :from (+ (range-from range) scalar)
	      :to (+ (range-to range) scalar)))

(defun range-= (a b)
  (and (= (range-from a) (range-from b))
       (= (range-to a) (range-to b))))

(defun range-from-< (a b)
  (< (range-from a) (range-from b)))

(defun make-range-from-width (from width)
  (make-range :from from :to (1- (+ from width))))

(defun range-disjoint-p (a b)
  (or (< (range-to a) (range-from b))
      (< (range-to b) (range-from a))))

(defun range-intersection (a b)
  (when (not (range-disjoint-p a b))
    (make-range :from (max (range-from a) (range-from b))
		:to (min (range-to a) (range-to b)))))

(defun range-union (a b)
  (make-range :from (min (range-from a) (range-from b))
	      :to (max (range-to a) (range-to b))))

(defun range-difference (a b)
  (cond ((range-disjoint-p a b) (list a b))
	((not (range-= a b))
	 (let ((union (range-union a b))
	       (intersection (range-intersection a b))
	       ranges)
	   (when (not (= (range-from union) (range-from intersection)))
	     (push (make-range :from (range-from union)
			       :to (1- (range-from intersection)))
		   ranges))
	   (when (not (= (range-to union) (range-to intersection)))
	     (push (make-range :from (1+ (range-to intersection))
			       :to (range-to union))
		   ranges))
	   ranges))))

(defun range-min (a b)
  (if (< (range-from a) (range-from b)) a b))

(defun range-max (a b)
  (if (> (range-to a) (range-to b)) a b))
