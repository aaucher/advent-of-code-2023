(defpackage #:advent2023.day07
  (:use :cl :day00))
(in-package #:advent2023.day07)

(defstruct hand cards type)

(defstruct hand-and-bid hand bid)

(defun parse-hand (word)
  (let ((cards (loop for c across word collect c)))
    (make-hand :cards cards :type (make-hand-type cards))))

(defun parse-bid (word)
  (parse-integer word))

(defun parse-hand-and-bid (line)
  (let ((words (str:words line)))
    (make-hand-and-bid :hand (parse-hand (first words))
		       :bid (parse-bid (second words)))))

(defparameter *with-joker-rule* nil)

(defparameter *strongest-to-weakest-cards*
  '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))

(defparameter *strongest-to-weakest-cards-with-joker*
  '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))

(defparameter *strongest-to-weakest-hands*
  '(:five-of-a-kind :four-of-a-kind :full-house-p :three-of-a-kind :two-pair :pair :high-card))

(defun strongest-to-weakest-cards (&key (*with-joker-rule* *with-joker-rule*))
  (if *with-joker-rule* *strongest-to-weakest-cards-with-joker* *strongest-to-weakest-cards*))

(defun high-card (hand)
    (reduce #'(lambda (l r)
		(if (< (position l (strongest-to-weakest-cards))
		       (position r (strongest-to-weakest-cards)))
		    l
		    r))
	    hand))

(defun make-hand-type (cards &key (*with-joker-rule* *with-joker-rule*))
  (let ((card-map (make-hash-table :size 5)))
    (labels ((joker-p ()
	       (let ((n (or (gethash #\J card-map) 0)))
		 (when (and *with-joker-rule* (> n 0)) n)))
	     (cards-by-number (&key no-joker)
	       (sort (loop for card being the hash-keys in card-map using (hash-value n)
			   when (or (not no-joker) (not (char= card #\J)))
			   collect (cons card n))
		     #'(lambda (l r) (cond ((> (cdr l) (cdr r)) t)
					   ((= (cdr l) (cdr r)) (card-< (car l) (car r)))))))
	     (five-of-a-kind-p ()
	       (when (or (= (hash-table-count card-map) 1)
			 (and *with-joker-rule*
			      (= (hash-table-count card-map) 2)
			      (joker-p)))
		 (first (cards-by-number))))
	     (four-of-a-kind-p ()
	       (or (and (= (hash-table-count card-map) 2)
			(= 4 (cdar (cards-by-number :no-joker t)))
			(first (cards-by-number)))
		   (and *with-joker-rule*
			(= (hash-table-count card-map) 3)
			(joker-p)
			(= 4 (+ (joker-p) (cdar (cards-by-number :no-joker t))))
			(first (cards-by-number :no-joker t)))))
	     (full-house-p ()
	       (let ((brelan (or (and (= (cdr (first (cards-by-number))) 3)
				      (first (cards-by-number)))
				 (and *with-joker-rule*
				      (= (hash-table-count card-map) 3)
				      (joker-p)
				      (= 3 (+ (joker-p)
					      (cdr (first (cards-by-number :no-joker t)))))
				      (first (cards-by-number :no-joker t)))))
		     (pair (or (and (= (cdar (last (cards-by-number))) 2)
				    (car (last (cards-by-number))))
			       (and *with-joker-rule*
				    (= (cdar (last (cards-by-number :no-joker t))) 2)
				    (car (last (cards-by-number :no-joker t)))))))
		 (when (and brelan pair)
		   (list brelan pair))))
	     (three-of-a-kind-p ()
	       (or (and (= (cdar (cards-by-number)) 3)
			(first (cards-by-number)))
		   (and *with-joker-rule*
			(= (hash-table-count card-map) 4)
			(joker-p)
			(= 3 (+ (joker-p)
				(cdr (first (cards-by-number :no-joker t)))))
			(first (cards-by-number :no-joker t)))))
	     (pairs ()
	       (remove-if-not #'(lambda (c) (= (cdr c) 2)) (cards-by-number)))
	     (two-pair-p ()
	       (let ((pairs (pairs)))
		 (when (or (= (length pairs) 2)
			   (and (= (length pairs) 1)
				(joker-p)))
		   pairs)))
	     (pair-p ()
	       (or (let ((pairs (pairs)))
		     (when (= (length pairs) 1)
		       (first pairs)))
		   (and (joker-p)
			(first (cards-by-number :no-joker t))))))
      (loop for card in cards do
	(setf (gethash card card-map) (1+ (or (gethash card card-map) 0))))
      (cond ((five-of-a-kind-p) (list :five-of-a-kind (car (five-of-a-kind-p))))
	    ((four-of-a-kind-p) (list :four-of-a-kind (car (four-of-a-kind-p))))
	    ((full-house-p) (list :full-house-p
				  (car (first (full-house-p)))
				  (car (second (full-house-p)))))
	    ((three-of-a-kind-p) (list :three-of-a-kind (car (three-of-a-kind-p))))
	    ((two-pair-p) (list :two-pair
				(car (first (two-pair-p)))
				(car (second (two-pair-p)))))
	    ((pair-p) (list :pair (car (pair-p))))
	    (t (list :high-card (high-card cards)))))))

(defun compare-cards (left-card right-card)
  (- (position right-card (strongest-to-weakest-cards) :test #'char=)
     (position left-card (strongest-to-weakest-cards) :test #'char=)))

(defun card-< (left-card right-card)
  (< 0 (compare-cards left-card right-card)))

(defun weakest (left-hand right-hand)
  (let* ((left-hand-type (hand-type left-hand))
	 (left-hand-strength (position (first left-hand-type) *strongest-to-weakest-hands*))
	 (right-hand-type (hand-type right-hand))
	 (right-hand-strength (position (first right-hand-type) *strongest-to-weakest-hands*)))
    (cond ((< left-hand-strength right-hand-strength) right-hand)
	  ((> left-hand-strength right-hand-strength) left-hand)
	  (t (or (loop for left-card in (hand-cards left-hand)
		       for right-card in (hand-cards right-hand)
		       for comp = (compare-cards left-card right-card)
		       when (< comp 0)
			 return left-hand
		       when (> comp 0)
			 return right-hand)
		 left-hand)))))

(defun hand-< (left-hand right-hand)
  (let ((weakest (weakest left-hand right-hand)))
    (if (equalp weakest left-hand)
	t
	nil)))

(defun ranked-hand-and-bids (hand-and-bids)
  (sort hand-and-bids #'(lambda (left right)
			  (hand-< (hand-and-bid-hand left)
				  (hand-and-bid-hand right)))))

(defun solve-problem-1 (filepath)
  (loop for hand-and-bid in (ranked-hand-and-bids (mapcar #'parse-hand-and-bid
							  (get-file filepath)))
	for rank from 1
	sum (* (hand-and-bid-bid hand-and-bid) rank)))

(solve-problem-1 #p"inputs/example07.txt") ;; 6440
(solve-problem-1 #p"inputs/day07.txt") ;; 256448566

(defun solve-problem-2 (filepath)
  (let ((*with-joker-rule* t))
    (solve-problem-1 filepath)))

(solve-problem-2 #p"inputs/example07.txt") ;; 5905
(solve-problem-2 #p"inputs/day07.txt") ;; 254412181
