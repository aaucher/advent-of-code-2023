(defsystem :advent2023
  :version "0.0.1"
  :description "Advent of code 2023"
  :author "Adrien Aucher"
  :license "MIT"
  :depends-on (:alexandria :str)
  :serial t
  :components ((:file "day01")
	       (:file "day02")))
