#lang racket

; Some common functions that are widely used
(provide inc dec square average atomic?)
(provide check-equal? test-equal? test-case)

(require rackunit)

(define average
  (λ (x y) (* .5 (+ x y))))

(define square
  (λ (x) (* x x)))

(define identity
  (λ (x) x))

(define inc
  (λ (x) (+ 1 x)))

(define dec
  (λ (x) (- x 1)))

(define atomic?
  (lambda (expr)
    (and (not (pair? expr)))))

(check-equal? (average 10 15) 12.5 "Test for average")
(check-equal? (square  15) 225 "Test for square")
(check-equal? (identity 'abc) 'abc "Test for identity")
(check-equal? (inc 36) 37 "Test for inc")
(check-equal? (dec 79) 78 "Test for dec")