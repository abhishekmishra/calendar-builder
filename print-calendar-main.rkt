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
(require "task-builder.rkt")

;(define start-date (greg:date 2017 1 1))
;(define end-date (greg:date 2017 12 31))
;
;(define task-builder (make-object task-builder%))
;(send task-builder add-daily-task "learn russian" start-date end-date)
;(send task-builder add-daily-task "read book" start-date end-date)
;(send task-builder add-daily-task "profit" start-date end-date)
;
;(send task-builder add-weekly-task "run" start-date end-date '(2 4 6))
;(send task-builder add-weekly-task "harmonica" start-date end-date '(1 2 3))
;
;(send task-builder add-monday-task "watch a movie" start-date end-date)
;
;(define (tasks-for-date d)
;  (send task-builder tasks-for-date d))
;
;(define today (greg:today))
;(define first-day-of-this-month (greg:-days today (- (greg:->day today) 1)))

(define task-reader (make-object task-reader% "sample-tasks.txt"))
(send task-reader read-file)
(define task-builder (send task-reader get-task-builder))
(define cal (make-object task-calendar%))

(define (tasks-for-date d)
  (send task-builder tasks-for-date d))

(send cal make-calendar (get-field start-date task-builder) tasks-for-date)
(send cal show-calendar-in-interactions)
(send cal print-calendar "calendar.jpg" 'jpeg)
