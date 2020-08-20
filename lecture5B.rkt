#lang racket
(require "common.rkt" "queue-imp.rkt")
(require compatibility/mlist)

; Author: Zilu Tian
; March 22, 2020

; Computational Objects

; A language for describing electrical system
; An event-driven simulation 
; Primitives and Means of Combination 
;(define a (make-wire))
;(define b (make-wire))
;(define c (make-wire))
;(define d (make-wire))
;(define e (make-wire))
;(define s (make-wire))
;
;(or-gate a b d)
;(and-gate a b c)
;(inverter c e)
;(and-gate d e s)

; Means of Abstraction
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
  'ok))
        

; Implementing primitives
; Help methods: logic operations
(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid signal" s)]))

(define (logical-and s1 s2)
  (cond [(or (= s1 0) (= s2 0)) 0]
        [(and (= s1 1) (= s2 1)) 1]
        [else (error "Invalid signal" s1 s2)]))

(define (logical-or s1 s2)
  (cond [(or (= s1 1) (= s2 1)) 1]
        [(and (= s1 0) (= s2 0)) 0]
        [else (error "Invalid signal" s1 s2)]))

(test-case
 "Check logical primitive logical operators"
 (check-equal? (logical-not 0) 1)
 (check-equal? (logical-not 1) 0)
 (check-equal? (logical-and 0 0) 0)
 (check-equal? (logical-and 1 0) 0)
 (check-equal? (logical-and 0 1) 0)
 (check-equal? (logical-and 1 1) 1)
 (check-equal? (logical-or 0 0) 0)
 (check-equal? (logical-or 1 0) 1)
 (check-equal? (logical-or 0 1) 1)
 (check-equal? (logical-or 1 1) 1)
 "Test cases for logical operators passed")

(define (inverter in out)
  (define (invert-in)
    (let [(new-value (logical-not (get-signal in)))]
      (after-delay inverter-delay
                   (λ ()
                     (set-signal! out new-value)) the-agenda)))
  (add-action! in invert-in)) ; tell the object input wire, when you change, tell me about it

(define (and-gate a1 a2 out)
  (define (and-action)
    (let [(new-value (logical-and (get-signal a1) (get-signal a2)))]
      (after-delay and-gate-delay
                   (λ ()
                     (set-signal! out new-value)) the-agenda)))
  (add-action! a1 and-action)
  (add-action! a2 and-action))

(define (or-gate a1 a2 out)
  (define (or-action)
    (let [(new-value (logical-or (get-signal a1) (get-signal a2)))]
      (after-delay or-gate-delay
                   (λ ()
                     (set-signal! out new-value)) the-agenda)))
  (add-action! a1 or-action)
  (add-action! a2 or-action))

(define (make-wire)
  (let [(signal 0) (action-procs '())]
    (define (set-my-signal! new-value)
      (cond [(= signal new-value) 'done]
            [else
             (set! signal new-value)
             (call-each action-procs)]))
    (define (accept-action-proc proc)
      (set! action-procs
            (cons proc action-procs))
      (proc)) ; call (proc) to kick off the simulation
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-proc]
            [else (error "Bad message" m)]))
    dispatch)) 

; helper methods 
(define (call-each procedures)
  (cond [(null? procedures) 'done]
        [else ((car procedures))
              (call-each (cdr procedures))]))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))


(define (after-delay delay action agenda-name)
  (add-to-agenda!
   (+ delay (current-time agenda-name))
   action
   agenda-name))

(define (propagate agenda-name)
  (if (empty-agenda? agenda-name)
      'done
      (begin
        ((first-agenda-item agenda-name)) ; procedure with no arg, action 
        (remove-first-agenda-item! agenda-name)
        (propagate agenda-name))))


(define (probe name wire agenda-name)
  (add-action! wire
               (λ ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time agenda-name))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

; SUM 0 NEW-VALUE=0 ; current value of sum at time 0 is new-value 0

; Agenda
; agenda is a 1D table of time segments sorted by time
; current time is stored at the head of the agenda

; constructor and selector for time-segment 
(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

; constructor 
(define (make-agenda) (mlist 0))
; selector 
(define (current-time agenda) (mcar agenda))
(define (segments agenda) (mcdr agenda))

(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))

(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                      (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (if (empty-queue? q)
        (delete-queue! q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let [(first-seg (first-segment agenda))]
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(test-case
 "Check for agenda"
 (define test-agenda (make-agenda))
 (check-equal? (empty-agenda? test-agenda) #t)
 (check-equal? (current-time test-agenda) 0)
 (add-to-agenda! 1 (λ () (begin (fprintf (current-output-port) "\n action 1") (+ 1 5))) test-agenda)
 (add-to-agenda! 2 (λ () (begin (fprintf (current-output-port) "\n action 2") (+ 2 5))) test-agenda)
; (check-equal? ((first-agenda-item test-agenda)) 6)
; (remove-first-agenda-item! test-agenda)
; (check-equal? (current-time test-agenda) 1)
; (check-equal? ((first-agenda-item test-agenda)) 7)
 (fprintf (current-output-port) " -Test for propagate")
 (propagate test-agenda)
 (check-equal? (current-time test-agenda) 2)
 "Test cases for agenda passed")

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input1 (make-wire))
(define input2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define the-agenda (make-agenda))

;(probe 'sum sum the-agenda)
;(probe 'carry carry the-agenda)
;(half-adder input1 input2 sum carry)
;(set-signal! input1 1)
;(propagate the-agenda)



                      
