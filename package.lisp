;;;; package.lisp

(defpackage #:aoc2019.intcode
  (:use #:cl #:aoc-util)
  (:nicknames #:intcode)
  (:export #:load-program
	   #:prog-running
	   #:set-parameters! #:send-input
	   #:execute-instruction! #:execute-program!
	   #:evaluate-program! #:run-till-output!
	   #:prog-outputs #:prog-inputs #:prog-memory
	   #:prog-relative-base #:prog-ip #:prog-running-p))

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

(defpackage #:aoc2019.day05
  (:use #:cl #:aoc-util #:intcode)
  (:nicknames #:day05))

(defpackage #:aoc2019.day06
  (:use #:cl #:aoc-util)
  (:nicknames #:day06))

(defpackage #:aoc2019.day07
  (:use #:cl #:aoc-util #:intcode)
  (:nicknames #:day07))

(defpackage #:aoc2019.day08
  (:use #:cl #:aoc-util)
  (:nicknames #:day08))

(defpackage #:aoc2019.day09
  (:use #:cl #:aoc-util #:intcode)
  (:nicknames #:day09))

(defpackage #:aoc2019.day10
  (:use #:cl #:aoc-util)
  (:nicknames #:day10))
