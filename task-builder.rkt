#lang racket

;; Author: Abhishek Mishra
;; Date: 9th May 2017
;;
;; A TASK BUILDER
;; --------------
;; - Define a new task with a given name
;; - Write a predicate for the task which returns
;;   for any date the task is valid, false
;;   otherwise
;; - Some simple predicates are provided, for e.g.
;;   * Runs on all days in a range of dates
;;   * Runs on all specified weekdays in a range of dates

(require (prefix-in greg: gregor))

(provide task-builder%)
(provide task%)
(provide task-reader%)

(define task-builder%
  (class object%
    (super-new)

    ; The tasks list
    (define tasks '())

    (init-field start-date end-date)

    ; Add a new task to the tasks list
    (define/public (add-task name predicate)
      (set! tasks (append tasks
                          (list
                           (make-object task%
                             name
                             (interval-check-creator
                              start-date
                              end-date
                              predicate))))))

    ; Getter for the task list
    (define/public (get-tasks)
      tasks)

    (define/public (tasks-for-date d)
      (map (lambda (task)
             (get-field task-name task))
           (filter (lambda (task)
                     ((get-field task-predicate task) d))
                   tasks)))
    
    (define (interval-check-creator start-date end-date predicate)
      (lambda (date)
        (if (and (greg:date<=? date end-date)
                 (greg:date>=? date start-date)
                 (predicate date))
            #t
            #f)))
    
    ; Add a daily task to the task list
    (define/public (add-daily-task name)
      (add-task name
                (lambda (date) #t)))

    ; Add a task for weekdays
    (define/public (add-weekly-task name for-weekdays)
      (add-task  name
                 (lambda (date)
                   (ormap (lambda (wd)
                            (eq? (greg:->iso-wday date) wd))
                          for-weekdays))))

    ; Add a task for Mondays
    (define/public (add-monday-task name)
      (add-weekly-task name '(1)))

    ; Add a task for Tuesdays
    (define/public (add-tuesday-task name)
      (add-weekly-task name '(2)))

    ; Add a task for Wednesdays
    (define/public (add-wednesday-task name)
      (add-weekly-task name '(3)))

    ; Add a task for Thursdays
    (define/public (add-thursday-task name)
      (add-weekly-task name '(4)))

    ; Add a task for Fridays
    (define/public (add-friday-task name)
      (add-weekly-task name '(5)))

    ; Add a task for Saturdays
    (define/public (add-saturday-task name)
      (add-weekly-task name '(6)))

    ; Add a task for Sundays
    (define/public (add-sunday-task name)
      (add-weekly-task name '(7)))
    ))

(define task%
  (class object%
    (super-new)

    (init-field [task-name "noname"]
                [task-predicate (lambda (date) #t)])))


(define task-reader%
  (class object%
    (super-new)

    (init-field input-file-path)
    
    ;tasks builder object
    (define task-builder #f)

    ;list of tasks strings read from file
    (define input-tasks-string-list #f)

    (define/public (read-file)
      (set! input-tasks-string-list
            (string-split (file->string input-file-path) "\n"))
      (let ((start-date (get-start-date))
            (end-date (get-end-date)))
        ;(displayln (format "start-date ~a, end-date ~a" start-date end-date))
        (set! task-builder (make-object task-builder% start-date end-date)))
      (read-tasks))

    ; get start date
    (define (get-start-date)
      (let* ((start-date-parts (string-split (car input-tasks-string-list) ":"))
             (start-date-prefix (string-trim (car start-date-parts)))
             (start-date-string (string-trim (cadr start-date-parts))))
        (if (equal? start-date-prefix "start-date")
            (greg:iso8601->date start-date-string)
            #f)))

    ; get end date
    (define (get-end-date)
      (let* ((end-date-parts (string-split (cadr input-tasks-string-list) ":"))
             (end-date-prefix (string-trim (car end-date-parts)))
             (end-date-string (string-trim (cadr end-date-parts))))
        (if (equal? end-date-prefix "end-date")
            (greg:iso8601->date end-date-string)
            #f)))

    (define (read-tasks)
      (for-each (lambda (t)
                  (let* ((text (string-trim t)))
                    (if (equal? text "")
                        (begin 
                          ;(displayln "empty line")
                          #f)
                        (let*
                            ((parts (string-split text ":"))
                             (task-name (string-trim (car parts)))
                             (task-predicate (string-trim (cadr parts))))
                          ;(displayln (format "read task -> ~a, ~a" task-name task-predicate))
                          (if (equal? task-predicate "everyday")
                              (send task-builder add-daily-task task-name)
                              (let* ((pred-parts (string-split task-predicate "->"))
                                     (pred-start (string-trim (car pred-parts)))
                                     (pred-rest (string-trim (cadr pred-parts))))
                                (if (equal? pred-start "weekdays")
                                    (let ((weekdays (string-split pred-rest ",")))
                                      (for-each (lambda (weekday)
                                                  (let ((wday (string-trim weekday)))
                                                    (dynamic-send task-builder
                                                                  (string->symbol
                                                                   (format "add-~a-task" wday))
                                                                  task-name)))
                                                weekdays))
                                    #f)))))))
                (cddr input-tasks-string-list)))

    (define/public (get-task-builder)
      task-builder)
    
    ))
; some test code for task-builder%

;(define tl (make-object task-builder%))
;(send tl add-task "task1")
;(send tl add-task "task2")
;(send tl get-tasks)

; some test code for task%
;(define tl (make-object task-builder%))
;
;(send tl add-task "task1" (lambda (date) #t))
;(send tl add-task "task2" (lambda (date)
;                            (if (eq? (greg:->iso-wday date) 1)
;                                #t
;                                #f)))
;
;(get-field task-name (first (send tl get-tasks)))
;(define tp (get-field task-predicate (first (send tl get-tasks))))

; should return true
;(tp (greg:today))

; Some test code to read tasks from a file.
;
