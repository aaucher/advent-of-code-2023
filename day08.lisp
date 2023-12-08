(defpackage #:advent2023.day08
  (:use :cl :day00))
(in-package #:advent2023.day08)

(defstruct instructions arr next)

(defstruct node id l r)

(defstruct network instructions nodes)

(defun parse-instructions (line)
  (make-instructions :arr line :next 0))

(defun next-instruction (instructions)
  (prog1 (aref (instructions-arr instructions) (instructions-next instructions))
    (setf (instructions-next instructions)
	  (rem (1+ (instructions-next instructions))
	       (length (instructions-arr instructions))))))

(defun parse-node (line)
  (let* ((parts (str:split " = " line))
	 (lr (str:split ", " (subseq (second parts)
				     1
				     (1- (length (second parts)))))))
    (make-node :id (first parts) :l (first lr) :r (second lr))))

(defun parse-nodes (lines)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for line in lines do
      (let ((node (parse-node line)))
	(setf (gethash (node-id node) ht) node)))
    ht))

(defun parse-network (lines)
  (make-network :instructions (parse-instructions (car lines))
		:nodes (parse-nodes (cdr lines))))

(defun steps-from-to (network from ending-pred)
  (let* ((instructions (copy-structure (network-instructions network)))
	 (nodes (network-nodes network))
	 (current-node (gethash from nodes)))
    (loop for step from 0
	  for instruction = (next-instruction instructions)
	  if (funcall ending-pred current-node)
	    return (cons current-node step)
	  else do
	    (setf current-node (gethash (if (char= #\L instruction)
					    (node-l current-node)
					    (node-r current-node))
					nodes)))))

(defun solve-problem-1 (filepath)
  (cdr (steps-from-to (parse-network (get-file filepath :skip-empty-lines t))
		      "AAA"
		      #'(lambda (node) (string= "ZZZ" (node-id node))))))

(solve-problem-1 #p"inputs/example08-1.txt") ;; 2
(solve-problem-1 #p"inputs/example08-2.txt") ;; 6
(solve-problem-1 #p"inputs/day08.txt") ;; 22199

(defun starting-node-p (node)
  (char= #\A (aref (node-id node) 2)))

(defun ending-node-p (node)
  (char= #\Z (aref (node-id node) 2)))

(defun nodes-satisfying (network pred)
  (let ((nodes (network-nodes network)))
    (loop for id being the hash-keys in (network-nodes network)
	  when (funcall pred (gethash id nodes))
	    collect (gethash id nodes))))

(defun starting-nodes (network)
  (nodes-satisfying network #'starting-node-p))

(defun ending-nodes (network)
  (nodes-satisfying network #'ending-node-p))
