(defpackage #:advent2023.day00
  (:use :cl)
  (:nicknames #:day00)
  (:export #:get-file))
(in-package #:advent2023.day00)

(defun get-file (filepath &key skip-empty-lines)
  "Read the content of the file at FILEPATH as a line of lines."
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil :eof)
	  while (not (eq line :eof))
          when (not (and skip-empty-lines (str:emptyp line)))
          collect line)))
