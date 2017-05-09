#lang racket

;; Author: Abhishek Mishra
;; Date: 9th May 2017
;;
;; A program to draw a monthly/weekly calendar
;; with a checkbox list of items for each date.
;;
;; I can then print it and then mark the checkboxes
;; with a pen.

(require racket/draw)
(require racket/gui)
(require (prefix-in greg: gregor))

(require "draw-calendar.rkt")

;; This function add a task by name
;; for a particular date
;; TODO
(define (add-task taskname date)
  #t)

(define (tasks-for-date d)
  '("learn russian" "prog practice" "harmonica" "run" "read book" "profit"))

(define today (greg:today))
(define first-day-of-this-month (greg:-days today (- (greg:->day today) 1)))

(define cal (make-object task-calendar%))
(send cal make-calendar first-day-of-this-month tasks-for-date)
(send cal show-calendar-in-interactions)
;write to png
;(send target save-file "calendar.png" 'png)


