#lang racket

; Author: Zilu Tian
; Date: March 16, 2020

; Compound Data
(require "common.rkt")

; Rational Number 
;Data Abstraction:
;
;Programming methodology of setting up data objects
;by postulating constructors and selectors to separate
;use from representation 
;
;add-rat sub-rat mult-rat          <Use>
;---------------------------------------
;make-rat num denom  <Abstraction Layer>
;---------------------------------------
;pairs                  <Representation>
;
;QoS for combination:
;- Is it closed under that means of combination?
;
;Procedure vs Data:
;- How different are they, really?
;
;Procedures are objects
;- Procedures are not just acts of doing sth. They are objects
;- (cons 12 27), (cons 31 49). a and b are substituted by actual values
;  when procedure is first built. Procedure exists as a lambda object

; Constructor 
(define make-rat
  (λ (x y)
    (let ([a (gcd x y)])
    (cons (/ x a)
          (/ y a)))))

; Selector (num, denom)
(define num
  (λ (x) (car x)))

(define denom
  (λ (x) (cdr x )))
;(num (make-rat 1 2))
;(denom (make-rat 4 6))

(define add-rat
  (λ(x y)
    (make-rat (+ (* (num x)(denom y))
                 (* (num y)(denom x)))
              (* (denom x)(denom y)))))

(define sub-rat
  (λ(x y)
    (add-rat x (neg-rat y))))

(define neg-rat
  (λ (x)
    (make-rat (- (num x)) (denom x))))
               
(define mult-rat
  (λ(x y)
    (make-rat (* (num x)(num y))
              (* (denom x)(denom y)))))

;(define A (make-rat 1 2))
;(define B (make-rat 1 4))
;(sub-rat A B)
;(add-rat A B)
;(mult-rat A B)

; Representing vectors in the plane
(define make-vector cons)

(define xcor car)

(define ycor cdr)

(define add-vec
  (λ (x y)
    (make-vector (+ (xcor x) (xcor y))
                 (+ (ycor x) (ycor y)))))

(define neg-vec
  (λ (x)
    (make-vector (- (xcor x))(- (ycor x)))))

(define sub-vec
  (λ (x y)
    (add-vec x (neg-vec y))))

(define scale-vec
  (λ (c x)
    (make-vector (* c (car x)) (* c (cdr x)))))

;(define C (make-vector 1 2))
;(define D (make-vector 2 1))
;(add-vec C D)
;(sub-vec C D)

; line segments, a pair of vectors 
(define make-seg
  (λ (p q) (cons p q)))

(define seg-start
  (λ (p) (car p)))

(define seg-end
  (λ (p) (cdr p)))

(define midpoint
  (λ (s)
    (let ((a (seg-start s))
          (b (seg-end s)))
      (scale-vec 0.5 (add-vec a b)))))

(define length
  (λ (s)
    (define dist (sub-vec (seg-end s) (seg-start s)))
    (let ([dx (xcor dist)]
          [dy (ycor dist)])
      (sqrt (+ (square dx)(square dy))))))
      
;(define pointA (make-vector 0 0))
;(define pointB (make-vector 2 2))
;(define segmentA (make-seg pointA pointB))
;(midpoint segmentA)
;(length segmentA)

; Blurring line of procedure and data 
(define cons-ow
  (λ (a b)
    (λ (pick)
      (cond ((= pick 1) a)
            ((= pick 2) b)))))

(define car-ow
  (λ (x)
    (x 1)))

(define cdr-ow
  (λ (x)
    (x 2)))

;(define cons1 (cons-ow 10 11))
;(car-ow cons1)
;(cdr-ow cons1)




    
    


