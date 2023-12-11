(defpackage #:advent2023.day11
  (:use :cl :day00))
(in-package #:advent2023.day11)

(defun galaxyp (c)
  (char= #\# c))

(defun parse-image (lines)
  (loop for line in lines
	for y from 0
	for galaxies = (loop for c across line
			     for x from 0
			     when (galaxyp c)
			       collect (cons x y))
	when galaxies
	  nconc galaxies))

(defun galaxy-x (galaxy)
  (car galaxy))

(defun galaxy-y (galaxy)
  (cdr galaxy))

(defun columns-with-galaxies (galaxies)
  (remove-duplicates (mapcar #'galaxy-x galaxies)))

(defun columns-without-galaxies (galaxies)
  (let* ((columns-with-galaxies (columns-with-galaxies galaxies))
	 (min (list-min columns-with-galaxies))
	 (max (list-max columns-with-galaxies)))
    (loop for x from min below max
	  when (not (member x columns-with-galaxies))
	    collect x)))

(defun rows-with-galaxies (galaxies)
  (remove-duplicates (mapcar #'galaxy-y galaxies)))

(defun rows-without-galaxies (galaxies)
  (let* ((rows-with-galaxies (rows-with-galaxies galaxies))
	 (min (list-min rows-with-galaxies))
	 (max (list-max rows-with-galaxies)))
    (loop for y from min below max
	  when (not (member y rows-with-galaxies))
	    collect y)))

(defun expand-x (galaxies scale-factor)
  (let ((columns-without-galaxies (columns-without-galaxies galaxies)))
    (loop for galaxy in (sort galaxies #'(lambda (a b)
					   (< (galaxy-x a) (galaxy-x b))))
	  collect (cons (+ (galaxy-x galaxy)
			   (* (count-if #'(lambda (c) (< c (galaxy-x galaxy)))
					columns-without-galaxies)
			      (1- scale-factor)))
			(galaxy-y galaxy)))))

(defun expand-y (galaxies scale-factor)
  (let ((rows-without-galaxies (rows-without-galaxies galaxies)))
    (loop for galaxy in (sort galaxies #'(lambda (a b)
					   (< (galaxy-y a) (galaxy-y b))))
	  collect (cons (galaxy-x galaxy)
			(+ (galaxy-y galaxy)
			   (* (count-if #'(lambda (c) (< c (galaxy-y galaxy)))
					rows-without-galaxies)
			      (1- scale-factor)))))))

(defun expand (galaxies scale-factor)
  (expand-y (expand-x galaxies scale-factor) scale-factor))

(defun distance-no-diagonal (galaxy-a galaxy-b)
  (+ (abs (- (galaxy-y galaxy-b) (galaxy-y galaxy-a)))
     (abs (- (galaxy-x galaxy-b) (galaxy-x galaxy-a)))))

(defun all-distances (galaxies)
  (loop for galaxy-a in galaxies
	for n from 1
	nconc (loop for galaxy-b in (subseq galaxies n)
		    collect (distance-no-diagonal galaxy-a galaxy-b))))

(defun solve-problem-1 (filepath)
  (reduce #'+ (all-distances (expand (parse-image (get-file filepath)) 2))))

(solve-problem-1 #p"inputs/example11.txt") ;; 374
(solve-problem-1 #p"inputs/day11.txt") ;; 9403026

(defun solve-problem-2 (filepath scale-factor)
  (reduce #'+ (all-distances (expand (parse-image (get-file filepath)) scale-factor))))

(solve-problem-2 #p"inputs/example11.txt" 10) ;; 1030
(solve-problem-2 #p"inputs/example11.txt" 100) ;; 8410
(solve-problem-2 #p"inputs/day11.txt" 1000000) ;; 543018317006
