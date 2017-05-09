#lang racket

;; Author: Abhishek Mishra
;; Date: 9th May 2017
;;
;; A program to draw a monthly/weekly calendar
;; with a checkbox list of items for each date.

(require racket/draw)
(require racket/gui)
(require (prefix-in greg: gregor))

(provide task-calendar%)

;; Some constants
;; Define calendar day box with
(define DAY-BOX-WIDTH 150)
(define DAY-BOX-HEIGHT 200)
(define HEADER-HEIGHT 50)

;(define draw-calendar-interface<%>
;  (interface () make-calendar show-calendar-in-interactions print-calendar))

(define task-calendar%
  (class object%
    (super-new)
    ;; Calendar functions
    (define target (make-bitmap (+ HEADER-HEIGHT (* DAY-BOX-HEIGHT 5)) (* DAY-BOX-WIDTH 7)))
    (define dc (new bitmap-dc% [bitmap target]))

    ; See documentation about smoothing
    ; This mode makes the pen fall on whole pixels
    (send dc set-smoothing 'aligned)

    ; Makes a box at 0,0 (used for each calendar day)
    ; with constant width/height
    (define (draw-day-box)
      (send dc draw-rectangle
            0 0 ; origin  
            DAY-BOX-WIDTH DAY-BOX-HEIGHT) ; width and height
      )

    ; Makes a box (used as a check-box)
    (define (draw-check-box)
      (send dc draw-rectangle
            0 5 ; origin  
            10 10) ; width and height
      )

    ; Makes a box for the header text
    (define (header-box)
      (send dc draw-rectangle
            0 0 ; origin  
            (* 7 DAY-BOX-WIDTH) HEADER-HEIGHT) ; width and height
      )

    ; Gets the start day for the calendar,
    ; which is the Monday of the week of the date provided
    (define (get-start-day-for-calendar d)
      (greg:-days d (- (greg:->iso-wday d) 1)))

    ; A utility function to get the date display string
    ; for the date box at the given column and row
    ;
    ; The dates start for the Monday of the week of
    ; the first day of the month
    (define (get-day-for-calendar-location start-day column row)
      (parameterize ([current-locale "en"])
        (greg:~t (greg:+days
                  (get-start-day-for-calendar start-day)
                  (+ column (* row 7)))
                 "E MMMM d")))

    ; Write the task linex alongwith checkboxes
    ; inside the day box
    ; TODO: removed hard-coded coords
    (define (write-lines-in-box lines)
      (if (eq? '() lines)
          #f
          (begin
            (send dc translate 0 20)
            (draw-check-box)
            (send dc draw-text (car lines) 15 0)
            (write-lines-in-box (cdr lines)))))
    
    ; Make a calendar on a given drawing context
    ; Start day is usually first day of the month.
    ;
    ; The calendar will start at the Monday of the
    ; week of the start day.
    ;
    ; tasks-for-date is a procedure which takes a
    ; date and returns a list of task names for the
    ; date.
    (define/public (make-calendar start-day tasks-for-date)
      (define start-state (send dc get-transformation))
      
      ;; Write the calendar header
      (header-box)
      (send dc translate (* 3 DAY-BOX-WIDTH) 0)
      (send dc
            draw-text
            (parameterize ([current-locale "en"])
              (greg:~t (get-start-day-for-calendar start-day)
                       "MMMM yyyy"))
            5
            1)

      (send dc translate (* -3 DAY-BOX-WIDTH) HEADER-HEIGHT)
  
      ; Save the original transformation of the
      ; drawing so that we can restore it at any point
      (define orig-transformation (send dc get-transformation))


      ; For 5 weeks write a day box for each weekday
      ; and add tasks relevant to them
      (for ([row (in-range 5)])
        (for ([column (in-range 7)])
          (send dc set-transformation orig-transformation)
          (send dc translate (* column DAY-BOX-WIDTH) (* row DAY-BOX-HEIGHT))
      
          (draw-day-box)
          (send dc draw-text (get-day-for-calendar-location start-day column row)  5 1)

          (send dc translate 20 0)
          (write-lines-in-box (tasks-for-date #f))))

      (send dc set-transformation start-state))

    ; Shows the calendar in the interactions window.
    ; Useful for debugging
    (define/public (show-calendar-in-interactions)
      ;show on interactions window
      (make-object image-snip% target))

    ; Create calendar at location given by filename
    (define/public (print-calendar filename type)
      (send target save-file filename type 100 #:unscaled? #t))
    ))



