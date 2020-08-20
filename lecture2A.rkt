#lang racket

; Author: Zilu Tian
; Date: March 15, 2020

; Higher-order Procedures
(require "common.rkt")

;The rights and privileges of first-class citizens
;To be named by variables
;To be passed as arguments to procedures
;To be returned as values of procedures
;To be incorporated into data structures

; Motivating example: to generalize the sum operation for different terms
(define sum-int
  (λ (lower upper)
    (if (> lower upper)
        0
        (+ lower (sum-int (inc lower) upper)))))

(define sum-square
  (λ (lower upper)
    (if (> lower upper)
        0
        (+ (square lower) (sum-square (inc lower) upper)))))    

(test-case
 "Check summation sum-int and sum-square"
 (check-equal? (sum-int 1 5) 15)
 (check-equal? (sum-square 1 5) 55)
 "Test cases for first-order function sum-int and sum-square passed")

; Recursive version 
(define sum-rec
  (λ (term next lower upper)
    (if (> lower upper)
        0
        (+ (term lower) (sum-rec term next (next lower) upper)))))

; Iterative version 
(define sum-it
  (λ (term next lower upper)
    (define iter
      (λ (lower ans)
        (if (> lower upper)
            ans
            (iter (next lower) (+ ans (term lower))))))
    (iter lower 0)))

(test-case
 "Check summation sum-it and sum-rec"
 (check-equal? (sum-it square inc 1 5) 55)
 (check-equal? (sum-it identity inc 1 5) 15)
 (check-equal? (sum-rec square inc 1 5) 55)
 (check-equal? (sum-rec identity inc 1 5) 15)
 (check-equal? (sum-rec (λ(x) (/ 1 (* x (+ x 2)))) (λ(x) (+ x 4)) 1 5) (/ 38 105)) ; Leibniz formula for approximating pi/8
 "Test cases for higher-order function sum-it and sum-rec passed")

; Rewrite the square root method
(define fixed-point
  (λ (f init)
    (define tolerance 0.0001)
    (define close-enough?
      (λ (a b)
        (if (< (abs (- a b)) tolerance)
            true
            false)))
    (define iter
      (λ (old new)
        (if (close-enough? old new)
            new
            (iter new (f new)))))
      (iter init (f init))))

(define average-damp
  (λ(f)
    (λ(x) (average (f x) x))))

(define sqrt
  (λ (x)
    (fixed-point (average-damp(λ(y)(/ x y)))
                 1.0)))

; Newton's method for finding square roots
(define sqrt-newton
  (define deriv
    (λ(f)
      (let ([dx 0.0001])
        (λ(x)(/ (- (f (+ x dx)) (f x)) dx)))))
  (define newton
    (λ (f init)
      (define df (deriv f))
      (fixed-point (λ(x) (- x (/ (f x) (df x)))) init)))
  (λ (x)
    (newton (λ(y) (- x (square y))) 1)))

(test-case
 "Check square root method using fixed point and average-damp"
 (check-equal? (round (sqrt 9)) 3.0)
 (check-equal? (round (sqrt 49)) 7.0)
 (check-equal? (round (sqrt-newton 9)) 3.0)
 (check-equal? (round (sqrt-newton 49)) 7.0)
 "Test cases for square root using fixed point and newton's method passed")





   