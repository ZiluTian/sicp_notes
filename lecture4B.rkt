#lang racket
(require "common.rkt" "table-imp.rkt")

; Author: Zilu Tian
; Date: March 20, 2020

; Generic Operators 


;          use
;----------------------------
;    generic operators 
;----------------------------
;     representation  
;      |          |
;      |          |
;
;Case:
;- Divisions with their own personcal record system in a linked satellite network
;
;; Complex number system
; add-complex, sub-complex, mult-complex, div-complex         
;----------------------------------------------------------------
; real-part, imag-part, magnitude, angle, make-polar, make-rect
;----------------------------------------------------------------  
;     rect                ||       polar

Typed Data:
- Have George and Martha sign their data

; mechanisms for manifest types 
(define attach-type
  (λ (type contents)
    (cons type contents)))

(define (type datum) (car datum))
(define (contents datum) (cdr datum))

; Rep 1: rect 
(define (make-rectangular real imag)
  (attach-type 'rectangular (cons real imag)))

(define make-polar-rect
  (λ (r a)
    (cons (* r (cos a)) (* r (sin a)))))

(define real-part-rect car)
(define imag-part-rect cdr)

(define magnitude-rect
  (λ (z)
    (sqrt (+ (square (car z))
             (square (cdr z))))))

(define angle-rect
  (λ (z)
    (atan (cdr z) (car z))))

; Rep 2: rect
; Can use namespace to separate two reps which can then keep the same names 
(define (make-polar mag ang)
  (attach-type 'polar (cons mag ang)))

(define magnitude-polar car)
(define angle-polar cdr)

(define make-rectangular-polar
  (λ (x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x))))

(define real-part-polar
  (λ (z)
    (* (magnitude-polar z) (cos (angle-polar z)))))

(define imag-part-polar
  (λ (z)
    (* (magnitude-polar z) (sin (angle-polar z)))))

; Generic Selectors
; Method 1: Dispatch on Type
;(define rectangular?
;  (λ (z)
;    (eq? (type z) 'rectangular)))
;
;(define polar?
;  (λ (z)
;    (eq? (type z) 'polar)))
; 
;(define (real-part z)
;  (cond ((rectangular? z) (real-part-rect (contents z)))
;        ((polar? z) (real-part-polar (contents z)))))


; Method 2
; Installing the rectangular/polar operations in table
(define complex-table (make-table))

(put 'rectangular 'real-part real-part-rect complex-table)
(put 'rectangular 'imag-part imag-part-rect complex-table)
(put 'rectangular 'magnitude magnitude-rect complex-table)
(put 'rectangular 'angle angle-rect complex-table)

(put 'polar 'real-part real-part-polar complex-table)
(put 'polar 'imag-part imag-part-polar complex-table)
(put 'polar 'magnitude magnitude-polar complex-table)
(put 'polar 'angle angle-polar complex-table)

(define (operate op obj)
  (let [(proc (get (type obj) op complex-table))]
    (if (not (null? proc))
        (proc (contents obj))
        'undefined)))

(define (real-part obj) (operate 'real-part obj))
(define (imag-part obj) (operate 'imag-part obj))
(define (magnitude obj) (operate 'magnitude obj))
(define (angle obj) (operate 'angle obj))

(test-case
 "Check complex table"
 (define A (make-polar 10 0.523599)) ; radians of 30 degree 
 (define B (make-rectangular 5 5))

 (check-equal? (magnitude A) 10)
 (check-equal? (angle A) 0.523599)
 (check-equal? (real-part A) 8.660252915835663)
 (check-equal? (round (imag-part A)) 5.0)
; (check-equal? 
 (check-equal? (real-part B) 5)
 (check-equal? (imag-part B) 5)
 (check-equal? (round (magnitude B)) 7.0)
 (check-equal? (angle B) 0.7853981633974483) ; radians of 45 degree
 "Test cases for complex table with typed data passed"
)

(define arith-table (make-table))

; Install ordinary numbers
(define (make-number n)
  (attach-type 'number n))

(define (add-number x y)
  (make-number (+ x y)))
(put 'number 'add add-number arith-table)

(define (sub-number x y)
  (make-number (- x y)))
(put 'number 'subtract sub-number arith-table)

(define (mult-number x y)
  (make-number (* x y)))
(put 'number 'mul mult-number arith-table)

(define (div-number x y)
  (make-number (/ x y)))
(put 'number 'div div-number arith-table)

; Install complex numbers
(define (make-complex n)
  (attach-type 'complex n))

(define add-complex
  (λ (z1 z2)
    (make-complex
     (make-rectangular (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))))
(put 'complex 'add add-complex arith-table)

(define sub-complex
  (λ (z1 z2)
    (make-complex
     (make-rectangular (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))))
(put 'complex 'subtract sub-complex arith-table)

(define mult-complex
  (λ (z1 z2)
    (make-complex
     (make-polar (* (magnitude z1) (magnitude z2))
                 (+ (angle z1) (angle z2))))))
(put 'complex 'mul mult-complex arith-table)

(define div-complex
  (λ (z1 z2)
    (make-complex 
     (make-polar (/ (magnitude z1) (magnitude z2))
                 (- (angle z1) (angle z2))))))
(put 'complex 'div div-complex arith-table)

;  Generic operators 
(define (add x y)
  ((get (type x) 'add arith-table) (contents x) (contents y)))
(define (sub x y)
  ((get (type x) 'subtract arith-table) (contents x) (contents y)))
(define (mul x y)
  ((get (type x) 'mul arith-table) (contents x) (contents y)))
(define (div x y)
  ((get (type x) 'div arith-table) (contents x) (contents y)))

(test-case
 "Check arithmetic table with generic operators"
 (check-equal? (add (make-number 4) (make-number 5)) (make-number 9))
 (check-equal? (add (make-complex (make-rectangular 10 10))
                    (make-complex (make-rectangular 5 5))) (make-complex (make-rectangular 15 15)))
 (check-equal? (add (make-complex (make-polar 10 0.523599))
                    (make-complex (make-polar 10 0.523599))) (make-complex (make-rectangular (real-part (make-polar 20 0.523599))
                                                                                             (imag-part (make-polar 20 0.523599)))))
 (check-equal? (sub (make-number 75.3) (make-number 5)) (make-number 70.3))
 (check-equal? (sub (make-complex (make-rectangular 8.5 10))
                    (make-complex (make-rectangular 5 5))) (make-complex (make-rectangular 3.5 5)))
 (check-equal? (sub (make-complex (make-polar 10 0.523599))
                    (make-complex (make-polar 10 0.523599))) (make-complex (make-rectangular 0.0 0.0)))
 (check-equal? (mul (make-number 24) (make-number 5)) (make-number 120))
 (check-equal? (mul (make-complex (make-polar 8 10))
                    (make-complex (make-polar 5 5))) (make-complex (make-polar 40 15)))
 (check-equal? (mul (make-complex (make-rectangular 8 10))
                    (make-complex (make-rectangular 5 5))) (make-complex (make-polar (magnitude (make-rectangular -10 90))
                                                                                     (angle (make-rectangular -10 90)))))
 (check-equal? (div (make-number 24) (make-number 8)) (make-number 3))
 (check-equal? (div (make-complex (make-polar 10 0.5))
                    (make-complex (make-polar 2 0.2))) (make-complex (make-polar 5 0.3)))
 (check-equal? (div (make-complex (make-rectangular 64 10))
                    (make-complex (make-rectangular 2 0))) (make-complex (make-polar (magnitude (make-rectangular 32 5))
                                                                                     (angle (make-rectangular 32 5)))))
 "Test cases for arithmetic table with generic operators that supports complex and ordinary numbers passed")
                 
;(polynomial X <term-list>)
; (x^15 + 2x^7 + 5)  
;((15 1) (7 2)) 

; Install polynomial 
(define add-poly
  (λ (p1 p2)
    (if (same-var? (var p1) (var p2))
        (make-polynomial
         (var p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var"))))

(put 'polynomial 'add add-poly arith-table)

(define (same-var? var1 var2)
  (eq? var1 var2))

(define make-polynomial
  (λ (var term-list)
    (attach-type 'polynomial (cons var term-list))))
                 
(define (var poly) (cadr poly))
(define (term-list poly) (cddr poly))

(define (empty-termlist? l) (null? l))
(define (first-term l) (car l))
(define (rest-terms l) (cdr l))

(define (make-term order coeff) (cons order coeff))
(define (order term) (car term))
(define (coeff term) (cdr term))

(define (adjoin-term term l)
  (append (list term) l))
  
(define add-terms
  (λ (l1 l2)
    (cond [(empty-termlist? l1) l2]
          [(empty-termlist? l2) l1]
          [else (let [(t1 (first-term l1))
                      (t2 (first-term l2))]
                  (cond
                    [(> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms l1) l2))]
                    [(< (order t1) (order t2))
                     (adjoin-term t2 (add-terms l1 (rest-terms l2)))]
                    [else
                     (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1)
                                      (coeff t2)))
                      (add-terms (rest-terms l1)
                                 (rest-terms l2)))]))])))

(test-case
 "Check for polynomial addition"
 (define poly1 (make-polynomial 'x (list (cons 1 (make-number 4)) (cons 2 (make-number 5)))))
 (define poly2 (make-polynomial 'x (list (cons 1 (make-number 8)) (cons 2 (make-number 10)))))
 (define poly3 (make-polynomial 'y (list (cons 5 (make-complex (make-rectangular 18 41))))))
 (define poly4 (make-polynomial 'y (list (cons 5 (make-complex (make-rectangular 36 82))))))
 (check-equal? (add-poly poly1 poly1) poly2)
 (check-equal? (add-poly poly3 poly3) poly4)
 "Test cases for polynomial addition with ordinary and complex numbers as coefficients passed"
 )
