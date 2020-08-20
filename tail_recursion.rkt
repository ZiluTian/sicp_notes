#lang racket

; Zilu Tian
; Lecture notes for SICP

;Recursive procedure can have iterative process or recursive process
;
;For languages where interpretation of any recursive procedures consumes
;an amount of memory that grows with the number of procedure calls,
;iterative processes have special loop constructs
;
;Tail-recursive: An implementation such that any iterative process consumes
;constant space, even if described by a recursive procedure


; Tree recursion 
; Counting change
; 
; How many different ways can we make change of 1,
; given 0.5, 0.25, 0.1, 0.05, and 0.01?

(define count-change
  (λ (amount)
    
    (define cc
      (λ (amount coin-type)
        (cond [(= amount 0) 1]
              [(or (< amount 0) (= coin-type 0)) 0]
              [else (+ (cc amount (- coin-type 1))
                       (cc (- amount (coin-value coin-type)) coin-type))])))

    (define coin-value
      (λ (coin-type)
        (cond [(= coin-type 1) 50]
              [(= coin-type 2) 25]
              [(= coin-type 3) 10]
              [(= coin-type 4) 5]
              [(= coin-type 5) 1])))
    
    (cc amount 5)))

;; floating point numbers suffer from precision  
;(define coin-value
;  (λ (coin-type)
;    (cond [(= coin-type 1) 0.5]
;          [(= coin-type 2) 0.25]
;          [(= coin-type 3) 0.1]
;          [(= coin-type 4) 0.05]
;          [(= coin-type 5) 0.01])))

(count-change 100)
               