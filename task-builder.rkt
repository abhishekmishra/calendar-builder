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

(define task-builder%
  (class object%
    (super-new)

    ; The tasks list
    (define tasks '())

    ; Add a new task to the tasks list
    (define/public (add-task name predicate)
      (set! tasks (append tasks
                          (list
                           (make-object task%
                             name
                             predicate)))))

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
    (define/public (add-daily-task name start-date end-date)
      (add-task name
                (interval-check-creator
                 start-date
                 end-date
                 (lambda (date) #t))))

    ; Add a task for weekdays
    (define/public (add-weekly-task name start-date end-date for-weekdays)
      (add-task  name
                 (interval-check-creator
                  start-date
                  end-date
                  (lambda (date)
                    (ormap (lambda (wd)
                             (eq? (greg:->iso-wday date) wd))
                           for-weekdays)))))

    ; Add a task for Mondays
    (define/public (add-monday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(1)))

    ; Add a task for Tuesdays
    (define/public (add-tuesday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(2)))

    ; Add a task for Wednesdays
    (define/public (add-wednesday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(3)))

    ; Add a task for Thursdays
    (define/public (add-thursday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(4)))

    ; Add a task for Fridays
    (define/public (add-friday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(5)))

    ; Add a task for Saturdays
    (define/public (add-saturday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(6)))

    ; Add a task for Sundays
    (define/public (add-sunday-task name start-date end-date)
      (add-weekly-task name start-date end-date '(7)))
    ))

(define task%
  (class object%
    (super-new)

    (init-field [task-name "noname"]
                [task-predicate (lambda (date) #t)])))


; some test code for task-builder%

;(define tl (make-object task-builder%))
;(send tl add-task "task1")
;(send tl add-task "task2")
;(send tl get-tasks)

; some test code for task%
(define tl (make-object task-builder%))

(send tl add-task "task1" (lambda (date) #t))
(send tl add-task "task2" (lambda (date)
                            (if (eq? (greg:->iso-wday date) 1)
                                #t
                                #f)))

(get-field task-name (first (send tl get-tasks)))
(define tp (get-field task-predicate (first (send tl get-tasks))))

; should return true
(tp (greg:today))
