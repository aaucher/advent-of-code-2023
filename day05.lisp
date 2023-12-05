(defpackage #:advent2023.day05
  (:use :cl :day00))
(in-package #:advent2023.day05)

(defstruct map-type from to)
(defstruct map-range source-range transformation)
(defstruct conversion-map type ranges)
(defstruct almanach seeds conversion-maps)

(defun parse-seeds (line)
  (mapcar #'parse-integer (str:split " " (subseq line 7))))

(defun parse-map-type (line)
  (let ((types (str:split "-to-" (car (str:split " " line)))))
    (make-map-type :from (alexandria:make-keyword (str:upcase (car types)))
		   :to (alexandria:make-keyword (str:upcase (cadr types))))))

(defun parse-range (line)
  (let ((range (mapcar #'parse-integer (str:split " " line))))
    (make-map-range :source-range (make-range-from-width (second range) (third range))
		    :transformation (- (first range) (second range)))))

(defun parse-conversion-map (lines)
  (let ((type (parse-map-type (car lines)))
	(ranges (loop for line in (cdr lines)
		      while (not (str:emptyp line))
		      collect (parse-range line))))
    (make-conversion-map :type type
			 :ranges (sort ranges
				       #'(lambda (a b)
					   (range-from-< (map-range-source-range a)
							 (map-range-source-range b)))))))

(defun parse-almanach (lines)
  (let ((seeds (parse-seeds (car lines)))
	(conversion-maps (mapcar #'parse-conversion-map
				 (split-when #'str:emptyp (cddr lines)))))
    (make-almanach :seeds seeds :conversion-maps conversion-maps)))

(defun convert-seed (seed conversion-map)
  (or (loop for range in (conversion-map-ranges conversion-map)
	    when (range-intersection (map-range-source-range range)
				     (make-range :from seed :to seed))
	      return (+ seed (map-range-transformation range)))
      seed))

(defun seed-to-location (seed conversion-maps)
  (reduce #'convert-seed conversion-maps :initial-value seed))

(defun locations (almanach)
  (mapcar #'(lambda (s) (seed-to-location s (almanach-conversion-maps almanach)))
	  (almanach-seeds almanach)))

(defun solve-problem-1 (filepath)
  (list-min (locations (parse-almanach (get-file filepath)))))

(solve-problem-1 #p"inputs/example05.txt") ;; 35
(solve-problem-1 #p"inputs/day05.txt") ;; 424490994

(defun seeds-as-range (seeds)
  (sort (mapcar #'(lambda (chunk)
		    (apply #'make-range-from-width chunk))
		(chunk seeds 2))
	#'range-from-<))

(defun solve-problem-2 (filepath)
  (parse-almanach (get-file filepath)))

(solve-problem-2 #p"inputs/example05.txt") ;; 46
(solve-problem-2 #p"inputs/day05.txt") ;;
