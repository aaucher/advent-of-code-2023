(defpackage #:advent2023.day07
  (:use :cl :day00))
(in-package #:advent2023.day07)

(defstruct hand-and-bid hand bid)

(defun parse-hand (word)
  (loop for c across word collect c))

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

(defun filter-map-entry-satisfying (ht pred &key (*with-joker-rule* *with-joker-rule*))
  (loop for c being the hash-keys in ht using (hash-value n)
	when (and (funcall pred n)
		  (or (not *with-joker-rule*)
		      (not (char= c #\J))))
	  collect (cons c n)))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(defun hand-type (hand &key (*with-joker-rule* *with-joker-rule*))
  (let ((cards (make-hash-table :size 5)))
    (labels ((highest-card-count ()
	       (first (sort (or (filter-map-entry-satisfying cards (lambda (n) (= n 5)))
				(filter-map-entry-satisfying cards (lambda (n) (= n 4)))
				(filter-map-entry-satisfying cards (lambda (n) (= n 3)))
				(filter-map-entry-satisfying cards (lambda (n) (= n 2)))
				(filter-map-entry-satisfying cards (lambda (n) (= n 1))))
			    #'(lambda (l r) (card-< (car l) (car r))))))
	     (joker-p ()
	       (let ((n (or (gethash #\J cards) 0)))
		 (when (and *with-joker-rule* n (> n 0)) n)))
	     (highest-card-count-with-joker (n)
	       (when (joker-p)
		 (<= n (+ (joker-p) (or (cdr (highest-card-count)) 0)))))
	     (five-of-a-kind-p ()
	       (or (filter-map-entry-satisfying cards (lambda (n) (= n 5)))
		   (and (highest-card-count-with-joker 5)
			(list (or (highest-card-count) (cons #\J 5))))))
	     (four-of-a-kind-p ()
	       (or (filter-map-entry-satisfying cards (lambda (n) (= n 4)))
		   (and (highest-card-count-with-joker 4)
			(list (highest-card-count)))))
	     (full-house-p ()
	       (let* ((brelan (or (filter-map-entry-satisfying cards (lambda (n) (= n 3)))
				  (and (highest-card-count-with-joker 3)
				       (list (highest-card-count)))))
		      (brelanless-cards (let ((ht (copy-hash-table cards)))
					  (remhash (caar brelan) ht)
					  ht))
		      (pair (filter-map-entry-satisfying brelanless-cards (lambda (n) (= n 2)))))
		 (when (and brelan pair)
		   (list brelan pair))))
	     (three-of-a-kind-p ()
	       (or (filter-map-entry-satisfying cards (lambda (n) (= n 3)))
		   (and (highest-card-count-with-joker 3)
			(list (highest-card-count)))))
	     (pairs ()
	       (filter-map-entry-satisfying cards (lambda (n) (= n 2))))
	     (two-pair-p ()
	       (let ((pairs (pairs)))
		 (when (or (= (length pairs) 2)
			   (and (= (length pairs) 1)
				(joker-p)))
		   pairs)))
	     (pair-p ()
	       (or (let ((pairs (pairs)))
		     (when (= (length pairs) 1)
		       pairs))
		   (and (joker-p) (list (highest-card-count))))))
      (loop for card in hand do
	(setf (gethash card cards) (1+ (or (gethash card cards) 0))))
      (cond ((five-of-a-kind-p) (list :five-of-a-kind (caar (five-of-a-kind-p))))
	    ((four-of-a-kind-p) (list :four-of-a-kind (caar (four-of-a-kind-p))))
	    ((full-house-p) (list :full-house-p
				  (caar (first (full-house-p)))
				  (caar (second (full-house-p)))))
	    ((three-of-a-kind-p) (list :three-of-a-kind (caar (three-of-a-kind-p))))
	    ((two-pair-p) (list :two-pair
				(car (first (two-pair-p)))
				(car (second (two-pair-p)))))
	    ((pair-p) (list :pair (caar (pair-p))))
	    (t (list :high-card (high-card hand)))))))

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
	  (t (or (loop for left-card in left-hand
		       for right-card in right-hand
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
