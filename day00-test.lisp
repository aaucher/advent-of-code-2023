(defpackage #:advent2023.day00-test
  (:use :cl :day00))
(in-package #:advent2023.day00-test)

(defun test-range-+ ()
  (assert (equalp (make-range :from 0 :to 10) (range-+ (make-range :from 0 :to 10) 0)))
  (assert (equalp (make-range :from 1 :to 11) (range-+ (make-range :from 0 :to 10) +1)))
  (assert (equalp (make-range :from -1 :to 9) (range-+ (make-range :from 0 :to 10) -1))))

(defun test-range-= ()
  (assert (range-= (make-range :from 0 :to 10) (make-range :from 0 :to 10)))
  (assert (not (range-= (make-range :from 0 :to 10) (make-range :from -10 :to 20))))
  (assert (not (range-= (make-range :from 0 :to 10) (make-range :from 2 :to 8))))
  (assert (not (range-= (make-range :from 0 :to 10) (make-range :from 5 :to 10))))
  (assert (not (range-= (make-range :from 0 :to 10) (make-range :from 0 :to 5)))))

(defun test-range-from-< ()
  (assert (range-from-< (make-range :from -10 :to 10) (make-range :from 0 :to 10)))
  (assert (range-from-< (make-range :from -10 :to 0) (make-range :from 0 :to 10)))
  (assert (range-from-< (make-range :from -10 :to -10) (make-range :from 10 :to 10)))
  (assert (not (range-from-< (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (not (range-from-< (make-range :from 0 :to 10) (make-range :from -10 :to 10))))
  (assert (not (range-from-< (make-range :from 10 :to 10) (make-range :from -10 :to -10)))))

(defun test-range-disjoint-p ()
  (assert (range-disjoint-p (make-range :from 0 :to 0) (make-range :from 10 :to 10)))
  (assert (range-disjoint-p (make-range :from 0 :to 10) (make-range :from 11 :to 21)))
  (assert (range-disjoint-p (make-range :from 0 :to 10) (make-range :from -11 :to -1)))
  (assert (not (range-disjoint-p (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (not (range-disjoint-p (make-range :from 0 :to 10) (make-range :from 5 :to 10))))
  (assert (not (range-disjoint-p (make-range :from 0 :to 10) (make-range :from 0 :to 5))))
  (assert (not (range-disjoint-p (make-range :from 0 :to 10) (make-range :from 2 :to 8))))
  (assert (not (range-disjoint-p (make-range :from 0 :to 10) (make-range :from -5 :to 5))))
  (assert (not (range-disjoint-p (make-range :from 0 :to 10) (make-range :from 5 :to 15)))))

(defun test-range-intersection ()
  (assert (equalp (make-range :from 0 :to 10)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from -10 :to 20))))
  (assert (equalp (make-range :from 2 :to 8)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from 2 :to 8))))
  (assert (equalp (make-range :from 5 :to 10)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from 5 :to 15))))
  (assert (equalp (make-range :from 0 :to 5)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from -5 :to 5))))
  (assert (equalp (make-range :from 10 :to 10)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from 10 :to 20))))
  (assert (equalp (make-range :from 0 :to 0)
		  (range-intersection (make-range :from 0 :to 10) (make-range :from -10 :to 0))))
  (assert (null (range-intersection (make-range :from 0 :to 10) (make-range :from 11 :to 21))))
  (assert (null (range-intersection (make-range :from 0 :to 10) (make-range :from -11 :to -1)))))

(defun test-range-union ()
  (assert (equalp (make-range :from 0 :to 10)
		  (range-union (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-union (make-range :from 2 :to 8) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-union (make-range :from 0 :to 10) (make-range :from 2 :to 8))))
  (assert (equalp (make-range :from 0 :to 15)
		  (range-union (make-range :from 0 :to 10) (make-range :from 5 :to 15))))
  (assert (equalp (make-range :from -5 :to 10)
		  (range-union (make-range :from 0 :to 10) (make-range :from -5 :to 5))))
  (assert (equalp (make-range :from 0 :to 20)
		  (range-union (make-range :from 0 :to 10) (make-range :from 10 :to 20))))
  (assert (equalp (make-range :from -10 :to 10)
		  (range-union (make-range :from 0 :to 10) (make-range :from -10 :to 0))))
  (assert (equalp (list (make-range :from 0 :to 10) (make-range :from 11 :to 21))
		  (range-union (make-range :from 0 :to 10) (make-range :from 11 :to 21))))
  (assert (equalp (list (make-range :from 0 :to 10) (make-range :from -11 :to -1))
		  (range-union (make-range :from 0 :to 10) (make-range :from -11 :to -1)))))

(defun test-range-difference ()
  (assert (null (range-difference (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (list (make-range :from 6 :to 10))
		  (range-difference (make-range :from 0 :to 5) (make-range :from 0 :to 10))))
  (assert (equalp (list (make-range :from 0 :to 4))
		  (range-difference (make-range :from 0 :to 10) (make-range :from 5 :to 10))))
  (assert (equalp (list (make-range :from 0 :to 1) (make-range :from 9 :to 10))
		  (range-difference (make-range :from 2 :to 8) (make-range :from 0 :to 10))))
  (assert (equalp (list (make-range :from 0 :to 1) (make-range :from 9 :to 10))
		  (range-difference (make-range :from 0 :to 10) (make-range :from 2 :to 8))))
  (assert (equalp (list (make-range :from 0 :to 4) (make-range :from 11 :to 15))
		  (range-difference (make-range :from 0 :to 10) (make-range :from 5 :to 15))))
  (assert (equalp (list (make-range :from -5 :to -1) (make-range :from 6 :to 10))
		  (range-difference (make-range :from 0 :to 10) (make-range :from -5 :to 5))))
  (assert (equalp (list (make-range :from 0 :to 9) (make-range :from 11 :to 20))
		  (range-difference (make-range :from 0 :to 10) (make-range :from 10 :to 20))))
  (assert (equalp (list (make-range :from -10 :to -1) (make-range :from 1 :to 10))
		  (range-difference (make-range :from 0 :to 10) (make-range :from -10 :to 0))))
  (assert (equalp (list (make-range :from 0 :to 10) (make-range :from 11 :to 21))
		  (range-difference (make-range :from 0 :to 10) (make-range :from 11 :to 21))))
  (assert (equalp (list (make-range :from 0 :to 10) (make-range :from -11 :to -1))
		  (range-difference (make-range :from 0 :to 10) (make-range :from -11 :to -1)))))

(defun test-range-min ()
  (assert (equalp (make-range :from 0 :to 10)
		  (range-min (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 0 :to 5)
		  (range-min (make-range :from 0 :to 10) (make-range :from 0 :to 5))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-min (make-range :from 0 :to 10) (make-range :from 5 :to 10))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-min (make-range :from 5 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-min (make-range :from 0 :to 10) (make-range :from 11 :to 21))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-min (make-range :from 11 :to 21) (make-range :from 0 :to 10)))))

(defun test-range-max ()
  (assert (equalp (make-range :from 0 :to 10)
		  (range-max (make-range :from 0 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 5 :to 10)
		  (range-max (make-range :from 0 :to 10) (make-range :from 5 :to 10))))
  (assert (equalp (make-range :from 5 :to 10)
		  (range-max (make-range :from 5 :to 10) (make-range :from 0 :to 10))))
  (assert (equalp (make-range :from 0 :to 10)
		  (range-max (make-range :from 0 :to 10) (make-range :from 0 :to 5))))
  (assert (equalp (make-range :from 11 :to 21)
		  (range-max (make-range :from 0 :to 10) (make-range :from 11 :to 21))))
  (assert (equalp (make-range :from 11 :to 21)
		  (range-max (make-range :from 11 :to 21) (make-range :from 0 :to 10)))))