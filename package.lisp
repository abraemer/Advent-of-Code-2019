;;;; package.lisp

(defpackage #:aoc2019.intcode
  (:use #:cl #:aoc-util)
  (:nicknames #:intcode)
  (:export #:load-program
	   #:define-intcode
	   #:set-parameters!
	   #:execute-instruction!
	   #:execute-program!
	   #:evaluate-program!))

(defpackage #:aoc2019.day01
  (:use #:cl #:aoc-util)
  (:nicknames #:day01))

(defpackage #:aoc2019.day02
  (:use #:cl #:aoc-util #:intcode)
  (:nicknames #:day02))

(defpackage #:aoc2019.day03
  (:use #:cl #:aoc-util)
  (:nicknames #:day03))

(defpackage #:aoc2019.day04
  (:use #:cl #:aoc-util)
  (:nicknames #:day04))
