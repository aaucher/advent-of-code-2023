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
  (let* ((type (parse-map-type (car lines)))
	 (ranges (loop for line in (cdr lines)
		       while (not (str:emptyp line))
		       collect (parse-range line)))
	 (catch-all (make-map-range :source-range (make-range :from 0 :to most-positive-fixnum)
				    :transformation 0)))
    (make-conversion-map :type type
			 :ranges (append ranges (list catch-all)))))

(defun parse-almanach (lines)
  (let ((seeds (parse-seeds (car lines)))
	(conversion-maps (mapcar #'parse-conversion-map
				 (split-when #'str:emptyp (cddr lines)))))
    (make-almanach :seeds seeds :conversion-maps conversion-maps)))

(defun convert-seed (seed conversion-map)
  (loop for range in (conversion-map-ranges conversion-map)
	when (range-intersection (map-range-source-range range)
				 (make-range :from seed :to seed))
	  return (+ seed (map-range-transformation range))))

(defun seed-to-location (seed conversion-maps)
  (reduce #'convert-seed conversion-maps :initial-value seed))

(defun locations (almanach)
  (mapcar #'(lambda (s) (seed-to-location s (almanach-conversion-maps almanach)))
	  (almanach-seeds almanach)))

(defun solve-problem-1 (filepath)
  (list-min (locations (parse-almanach (get-file filepath)))))

(solve-problem-1 #p"inputs/example05.txt") ;; 35
(solve-problem-1 #p"inputs/day05.txt") ;; 424490994

(defun transform-range (range map-range)
  (let* ((source-range (map-range-source-range map-range))
	 (transformation (map-range-transformation map-range))
	 (intersection (range-intersection range source-range)))
    (if intersection
	(values (range-+ intersection transformation)
		(remove-if #'(lambda (r)
			       (or (null r) (range-disjoint-p r range)))
			   (range-difference range source-range)))
	(values nil (list range)))))

(defun convert-range (initial-range conversion-map)
  (let ((ranges (list initial-range))
	converted-ranges)
    (loop for map-range in (conversion-map-ranges conversion-map) do
      (setf ranges (loop for range in ranges
			 nconc (multiple-value-bind (converted not-converted)
				   (transform-range range map-range)
				 (when converted
				   (push converted converted-ranges))
				 not-converted))))
    (nreverse converted-ranges)))

(defun seed-range-to-location-range (initial-seed-range almanach)
  (let ((ranges (list initial-seed-range)))
    (loop for conversion-map in (almanach-conversion-maps almanach) do
      (setf ranges (loop for range in ranges
			 nconc (convert-range range conversion-map))))
    ranges))

(defun seeds-as-range (seeds)
  (mapcar #'(lambda (chunk)
	      (apply #'make-range-from-width chunk))
	  (chunk seeds 2)))

(defun seeds-to-location (almanach)
  (loop for seed-range in (seeds-as-range (almanach-seeds almanach))
	nconc (seed-range-to-location-range seed-range almanach)))

(defun solve-problem-2 (filepath)
  (reduce #'range-min (seeds-to-location (parse-almanach (get-file filepath)))))

(solve-problem-2 #p"inputs/example05.txt") ;; 46
(solve-problem-2 #p"inputs/day05.txt") ;; 15290096
