(defpackage #:advent2023.day10
  (:use :cl :day00))
(in-package #:advent2023.day10)

(defparameter *pipes*
  '((#\| . (:north :south))
    (#\- . (:east :west))
    (#\L . (:north :east))
    (#\J . (:north :west))
    (#\7 . (:south :west))
    (#\F . (:south :east))
    (#\S . (:north :south :east :west))))

(defun opposite-direction (direction)
  (case direction
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)))

(defun starting-pipe-p (c)
  (cond ((tile-p c) (starting-pipe-p (tile-pipe c)))
	((characterp c) (char= c #\S))))

(defun pipe-p (c)
  (assoc c *pipes*))

(defun connect-to-direction (pipe)
  (cdr (assoc pipe *pipes*)))

(defstruct grid rows start)

(defstruct coord x y)

(defstruct tile pipe coord connect-to)

(defun move (direction coord)
  (let ((x (coord-x coord))
	(y (coord-y coord)))
  (case direction
    (:north (make-coord :x x :y (1- y)))
    (:south (make-coord :x x :y (1+ y)))
    (:east (make-coord :x (1+ x) :y y))
    (:west (make-coord :x (1- x) :y y)))))

(defun tile-at (rows x y)
  (when (and (>= x 0) (< x (length (aref rows y)))
	     (>= y 0) (< y (length rows)))
    (aref (aref rows y) x)))

(defun tile-at-coord (rows coord)
  (tile-at rows (coord-x coord) (coord-y coord)))

(defun parse-row-as-list (y line)
  (loop for c across line
	for x from 1
	for coord = (make-coord :x x :y y)
	if (pipe-p c)
	  collect (make-tile :pipe c :coord coord
			     :connect-to (mapcar #'(lambda (d)
						     (cons d (move d coord)))
						 (connect-to-direction c)))
	else
	  collect (make-tile :coord coord)))

(defun parse-row (y line)
  (coerce (append (list (make-tile :coord (make-coord :x 0 :y y)))
		  (parse-row-as-list y line)
		  (list (make-tile :coord (make-coord :x (1+ (length line)) :y y))))
	  'vector))

(defun parse-rows-as-list (lines)
  (loop for line in lines
	for y from 1
	collect (parse-row y line)))

(defun row-of-ground (y width)
  (coerce (loop for x from 0 upto (1+ width)
		collect (make-tile :coord (make-coord :x x :y y)))
	  'vector))

(defun find-start (rows)
  (loop for y from 0 below (length rows) do
    (loop for x from 0 below (length (aref rows y))
	  when (starting-pipe-p (tile-at rows x y))
	    do (return-from find-start (make-coord :x x :y y)))))

(defun remove-not-connected (grid)
  (let ((rows (grid-rows grid)))
    (loop for y from 0 below (length rows) do
      (loop for x from 0 below (length (aref rows y))
	    for tile = (tile-at rows x y)
	    do (setf (tile-connect-to tile)
		     (remove-if #'(lambda (connection)
				    (let ((other (tile-at-coord rows (cdr connection))))
				      (not (assoc (opposite-direction (car connection))
						  (tile-connect-to other)))))
				(tile-connect-to tile))))))
  grid)

(defun parse-grid (lines)
  (let ((rows (coerce (append (list (row-of-ground 0 (length (first lines))))
			      (parse-rows-as-list lines)
			      (list (row-of-ground (1+ (length lines)) (length (first lines)))))
		      'vector)))
     (remove-not-connected (make-grid :rows rows :start (find-start rows)))))

(defun loop-path (grid)
  (let* ((start-coord (grid-start grid))
	 (rows (grid-rows grid))
	 (start (tile-at-coord rows start-coord))
	 (next-connection (first (tile-connect-to start)))
	 (current (tile-at-coord rows (cdr next-connection))))
    (cons start
      (loop while (not (starting-pipe-p current))
	    collect current
	    do (setf next-connection (first (remove-if #'(lambda (connection)
							   (eq (opposite-direction (car next-connection))
							       (car connection)))
						       (tile-connect-to current))))
	       (setf current (tile-at-coord rows (cdr next-connection)))))))

(defun solve-problem-1 (filepath)
  (/ (length (loop-path (parse-grid (get-file filepath)))) 2))

(solve-problem-1 #p"inputs/example10-1.txt") ;; 8
(solve-problem-1 #p"inputs/day10.txt") ;; 6903

(defun set= (set-a set-b)
  (null (set-exclusive-or set-a set-b)))

(defun enclosed (grid)
  (let* ((loop-path (loop-path grid))
	 (on-loop-path (let ((ht (make-hash-table :size (length loop-path))))
			 (loop for tile in loop-path do
			   (setf (gethash (tile-coord tile) ht) t))
			 ht))
	 enclosed)
    (loop for row across (grid-rows grid) do
      (let ((loop-boundary-crossed 0)
	    last-westbound-crossing)
	(loop for tile across row do
	  (if (not (gethash (tile-coord tile) on-loop-path))
	      (when (oddp loop-boundary-crossed)
		(push tile enclosed))
	      (let ((connections (mapcar #'car (tile-connect-to tile))))
		(cond ((set= connections '(:north :south))
		       (incf loop-boundary-crossed))
		      ((set= connections '(:west :north))
		       (if (set= last-westbound-crossing '(:east :north))
			   (setf loop-boundary-crossed (+ 2 loop-boundary-crossed)
				 last-westbound-crossing nil)
			   (incf loop-boundary-crossed)))
		      ((set= connections '(:west :south))
		       (if (set= last-westbound-crossing '(:east :south))
			   (setf loop-boundary-crossed (+ 2 loop-boundary-crossed)
				 last-westbound-crossing nil)
			   (incf loop-boundary-crossed)))
		      ((or (set= connections '(:east :north))
			   (set= connections '(:east :south)))
		       (setf last-westbound-crossing connections))))))))
    enclosed))

(defun solve-problem-2 (filepath)
  (length (enclosed (parse-grid (get-file filepath)))))

(solve-problem-2 #p"inputs/example10-2.txt") ;; 10
(solve-problem-2 #p"inputs/day10.txt") ;; 265
