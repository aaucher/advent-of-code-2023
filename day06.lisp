(defpackage #:advent2023.day06
  (:use :cl :day00))
(in-package #:advent2023.day06)

(defstruct race time distance)

(defun parse-space-separated-list-of-numbers (str)
  (with-input-from-string (stream str)
    (loop for n = (read stream nil)
	  while n
	  collect n)))

(defun parse-times (line)
  (parse-space-separated-list-of-numbers (subseq line 5)))

(defun parse-distances (line)
  (parse-space-separated-list-of-numbers (subseq line 9)))

(defun parse-races (lines)
  (let ((times (parse-times (first lines)))
	(distances (parse-distances (second lines))))
    (mapcar #'(lambda (time distance)
		(make-race :time time :distance distance))
	    times distances)))

(defun least-hold-time-to-win (race)
  (let ((race-time (race-time race))
	(race-distance (race-distance race)))
    (loop for hold-time from 1 upto race-time
	  when (> (* hold-time (- race-time hold-time)) race-distance)
	    return hold-time)))

(defun number-of-ways-to-win (race)
  (let ((race-time (race-time race)))
    (* 2 (- (/ (1+ race-time) 2) (least-hold-time-to-win race)))))

(defun solve-problem-1 (filepath)
  (reduce #'* (mapcar #'number-of-ways-to-win (parse-races (get-file filepath)))
	  :initial-value 1))

(solve-problem-1 #p"inputs/example06.txt") ;; 288
(solve-problem-1 #p"inputs/day06.txt") ;; 771628

(defun parse-all-as-one-integer (str)
  (parse-integer (apply #'str:concat (str:words str))))

(defun parse-all-as-one-race (lines)
  (let ((time (parse-all-as-one-integer (subseq (first lines) 5)))
	(distance (parse-all-as-one-integer (subseq (second lines) 9))))
    (make-race :time time :distance distance)))

(defun solve-problem-2 (filepath)
  (number-of-ways-to-win (parse-all-as-one-race (get-file filepath))))

(solve-problem-2 #p"inputs/example06.txt") ;; 71503
(solve-problem-2 #p"inputs/day06.txt") ;; 27363861
