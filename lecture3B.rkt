#lang racket

; Author: Zilu Tian
; Date: March 18, 2020

; Symbolic Differentiation; Quotation 
(require "common.rkt")

; Derivative Rules: reduction

;Ambiguation:
;- can't substitute in referentially opaque context 
;- quotation is an example of such
;
;"Chicago" has seven letters
;Chicago is the biggest city in Illinois
;---------------------------------------------------
;"the biggeste city in Illinois" has seven letters


; set of rules
; Procedures represent local rules for the expansion of the process
; Dispatch on type

(define deriv
  (λ (expr var)
    (cond [(constant? expr var) 0]
          [(same-var? expr var) 1]
          [(sum? expr) (make-sum (deriv (A1 expr) var)
                                 (deriv (A2 expr) var))]
          [(product? expr) (make-sum
                            (make-product (M1 expr)
                                          (deriv (M2 expr) var))
                            (make-product (deriv (M1 expr) var)
                                          (M2 expr)))])))

(define constant?
  (λ (expr var)
    (and (atomic? expr)
         (not (eq? expr var)))))

(define same-var?
  (λ (expr var)
    (and (symbol? expr)
         (eq? expr var))))

; quotation
; sum: a non-atomic expression starting with symbol + 
(define sum?
  (λ (expr)
    (and (not (atomic? expr))
         (eq? (car expr) '+))))

(define make-sum
  (λ (a1 a2)
    (cond [(eq? 0 a1) a2]
          [(eq? 0 a2) a1]
          [(and (number? a1) (number? a2)) (+ a1 a2)]
          [else (list '+ a1 a2)])))

(define A1 cadr)
(define A2 caddr)

(define product?
  (λ (expr)
    (and (not (atomic? expr))
         (eq? (car expr) '*))))

(define make-product
  (λ (a1 a2)
    (cond [(or (eq? 0 a1) (eq? 0 a2)) 0]
          [(and (number? a1) (number? a2)) (* a1 a2)]
          [(eq? 1 a1) a2]
          [(eq? 1 a2) a1]
          [else (list '* a1 a2)])))
    

(define M1 cadr)
(define M2 caddr)

; Tests
(test-case
 "Check atomic?"
 (check-equal? (atomic? 'x) #t)
 (check-equal? (atomic? '10) #t)
 (check-equal? (atomic? '(+ x y)) #f)
 (check-equal? (atomic? '(* 1 (+ y x))) #f)
 "Test cases for atomic? passed")

(test-case
 "Check constant?"
 (check-equal? (constant? 10 'x) #t)
 (check-equal? (constant? 'x 'x) #f)
 (check-equal? (constant? '(* 10 x) 'y) #f)
 (check-equal? (constant? '(* 10 x) 'x) #f)
 "Test cases for constant? passed")

(test-case
 "Check same-var?"
 (check-equal? (same-var? 'x 'x) #t)
 (check-equal? (same-var? 'x 'y) #f)
 "Test cases for same-var? passed")

(test-case
 "Check sum?"
 (check-equal? (sum? '(+ x y)) #t)
 (check-equal? (sum? '(* (+ x a) y)) #f)
 "Test cases for sum? passed")

(test-case
 "Check product?"
 (check-equal? (product? '(* x y)) #t)
 (check-equal? (product? '(+ (* x a) y)) #f)
 "Test cases for product? passed")

(test-case
 "Check derivative result"
 (define exp0 '(+ (* a (* x x)) (+ (* b x) c)))
 (define exp1 '(* a x))
 (define exp2 '(* b (* x x)))
 (define exp3 '(* (+ c d) x))
 
 (check-equal? (deriv '(+ (* 2 x) x) 'x) '3)
 (check-equal? (deriv exp1 'x) 'a)
 (check-equal? (deriv exp1 'y) '0)
 (check-equal? (deriv exp2 'x) '(* b (+ x x)))
 (check-equal? (deriv exp3 'x) '(+ c d))
 "Test cases for deriv passed")


    





