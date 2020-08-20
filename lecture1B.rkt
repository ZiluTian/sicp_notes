#lang racket

; Author: Zilu Tian
; Date: March 15, 2020

; Substitution Model
; How procedures yield processes
(require "common.rkt")

; Expressions: 
; Numbers, Symbols, Lambda-expressions, Definitions, Conditionals, and Combinations

;Substitution Rule
;To evaluate an application
;- Evaluate the operator to get procedure
;- Evaluate the operands to get arguments
;- Apply the procedure to the arguments

; Peano arithmetic

; Both examples are recursive definitions
; Lead to different shapes of processes (perturbation analysis) 
; =======================================
; Time: O(x), Space: O(1)
; Iteration
; A system whose states are captured by explicit variables 
(define peano-add-it
  (λ (x y)
    (if (= x 0)
        y
        (peano-add-it (dec x) (inc y)))))

; Time: O(x), Space: O(x)
; Recursion
; A system whose states also involve internal variables 
(define peano-add-rec
  (λ (x y)
    (if (= x 0)
        y
        (inc (peano-add-rec (dec x) y)))))

(test-case
 "Check peano-add-it and peano-add-rec"
 (check-equal? (peano-add-it 3 5) 8)
 (check-equal? (peano-add-rec 3 5) 8)
 "Test cases for peano-add-it and peano-add-rec passed")
 
; Time: O(fib(x)). Exponential 
; Space: O(x) 
(define fib-rec
  (λ (x)
    (if (< x 2)
        x
        (+ (fib-rec (- x 1))
           (fib-rec (- x 2))))))

; Time: O(x), Space: O(1)
; Iteration 
(define fib-it
  (λ (x)
    (define iter
      (λ (iterations a b)
        (if (= 2 iterations)
            (+ a b)
            (iter (- iterations 1) b (+ a b)))))
    (if (< x 2)
        x
        (iter x 0 1))))

(test-case
 "Check fib-it and fib-rec"
 (check-equal? (fib-it 8) 21)
 (check-equal? (fib-rec 8) 21)
 "Test cases for fib-it and fib-rec passed")
      
; Hanoi Tower
(define print-move
  (lambda (from to)
    (fprintf (current-output-port)
             "From ~a To ~s \n"
             from
             to)))

(define move
  (lambda (n from to spare)
    (cond ((= 0 n) true)
          (else 
           (move (dec n) from spare to)
           (print-move from to)
           (move (dec n) spare to from)))))

(test-case
 "Check hanoi-tower example"
 (check-equal? (move 4 1 2 3) #t)
 "Test cases for Hanoi tower passed")
 
        
        

