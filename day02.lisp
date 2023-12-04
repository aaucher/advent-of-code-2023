(defpackage #:advent2023.day02
  (:use :cl :day00))
(in-package #:advent2023.day02)

(defstruct game number sets)

(defun parse-game-number (str)
  (parse-integer str :start 5))

(defun parse-cubes (str)
  (multiple-value-bind (n color-starts-at) (parse-integer str :junk-allowed t)
    (cons n (alexandria:make-keyword (str:upcase (subseq str (1+ color-starts-at)))))))

(defun parse-game-set (str)
  (mapcar #'parse-cubes (str:split ", " str)))

(defun parse-game-sets (str)
  (mapcar #'parse-game-set (str:split "; " str)))

(defun parse-game (str)
  (let ((game (str:split ": " str)))
    (make-game :number (parse-game-number (car game))
	       :sets (parse-game-sets (cadr game)))))

(defun cube-possible-p (cube)
  (let ((n (car cube)))
    (case (cdr cube)
      (:blue (<= n 14))
      (:green (<= n 13))
      (:red (<= n 12)))))

(defun set-possible-p (set)
  (every #'cube-possible-p set))

(defun game-possible-p (game)
  (every #'set-possible-p (game-sets game)))

(defun solve-problem-1 (filepath)
  (reduce #'+ (mapcar #'(lambda (line)
			  (let ((game (parse-game line)))
			    (if (game-possible-p game)
				(game-number game)
				0)))
		      (get-file filepath))))

(solve-problem-1 #P"inputs/example02.txt") ;; 8
(solve-problem-1 #P"inputs/day02.txt") ;; 2176

(defun get-cube (set color)
  (find-if #'(lambda (cube) (eq color (cdr cube))) set))

(defun get-cubes (sets color)
  (remove-if #'null (mapcar #'(lambda (set) (get-cube set color)) sets)))

(defun max-list (list)
  (apply #'max list))

(defun cube-power (sets)
  (let ((b (or (max-list (mapcar #'car (get-cubes sets :blue))) 1))
	(g (or (max-list (mapcar #'car (get-cubes sets :green))) 1))
	(r (or (max-list (mapcar #'car (get-cubes sets :red))) 1)))
    (* r g b)))

(defun solve-problem-2 (filepath)
  (reduce #'+ (mapcar #'(lambda (line)
			  (cube-power (game-sets (parse-game line))))
		      (get-file filepath))))

(solve-problem-2 #P"inputs/example02.txt") ;; 2286
(solve-problem-2 #P"inputs/day02.txt") ;; 63700
