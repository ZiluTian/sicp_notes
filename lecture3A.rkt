#lang racket

; Author: Zilu Tian
; Date: March 17, 2020

; Henderson Escher Example
(require "common.rkt")

; List operations: map, foreach
; Metalinguistic Abstraction: Peter Henderson's picture language 
;Language of Schemes of Combination
;==================================================================
;Language of Geometric Positions: above, beside, right-push, rotate 
;==================================================================
;Language of Primitive Picts: segments, points, unit square
;
;Different vocabulary for talking about designs at different levels 
;At each level, objects been discussed are the things defined in the previous layer
;Each layer is not setup for a particular task. Have a full range of linguistic power
;More robust: the solution space is continuous 
;
;Alternative design scheme
;Each decomposition is designed for a particular task
;Less robust
;     o
;    / \
;   /   \
;  o     o
;   \   / \
;    o o   o


; General method for applying a method mtd to all elements of l

(define map-it
  (λ (mtd l)
    (define iter
      (λ (q ans)
        (if (null? q)
            ans
            (iter (cdr q) (append ans (list (mtd (car q))))))))
    (iter l null)))

(define map-rec
  (λ (mtd l)
    (if (null? l)
        null
        (cons (mtd (car l))
              (map-rec mtd (cdr l))))))

(test-case
 "Check map-it and map-rec"
 (check-equal? (map-rec square (list 5 3 1)) (list 25 9 1))
 (check-equal? (map-rec square (list 0.5 0.9 1.2)) (list 0.25 0.81 1.44))
 (check-equal? (map-rec (λ (x) (* x 5)) (list 10 20 30)) (list 50 100 150))
 (check-equal? (map-it square (list 5 3 1)) (list 25 9 1))
 (check-equal? (map-it square (list 0.5 0.9 1.2)) (list 0.25 0.81 1.44))
 "Test cases for map-it and map-rec passed")

; General pattern for doing things over a list 
(define scale-list
  (λ (c l)
    (if (null? l)
        null
        (cons (* (car l) c)
              (scale-list c (cdr l))))))

(define scale-list-ow
  (λ (c l)
    (map-rec (lambda (x) (* x c)) l)))

(test-case
 "Check scale-list and scale-list-ow"
 (check-equal? (scale-list-ow 20 (list 5 3 1)) (list 100 60 20))
 (check-equal? (scale-list-ow 5 (list 0.5 0.9 1.2)) (list 2.5 4.5 6.0))
 (check-equal? (scale-list 20 (list 5 3 1)) (list 100 60 20))
 (check-equal? (scale-list 5 (list 0.5 0.9 1.2)) (list 2.5 4.5 6.0))
 "Test cases for scale-list and scale-list-ow passed")

(define foreach-it
  (λ (mtd l)
    (cond ((null? l) true)
          (else (mtd (car l))
              (foreach-it mtd (cdr l))))))

; Metalinguistic Abstraction
; Peter Henderson's picture language 
; http://www.fssnip.net/aj/title/Peter-Hendersons-picture-language-from-SICP

; http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/

; means of abstraction:

; Language is embedded in LISP
; primitive: picture
; means of combination: rotate, beside, flip, above (closure: closed)
; Use procedures as means of combination
; For means of abstraction, everything LISP supplies for manipulating procedures
; is available
; => Language is embedded in LISP: all the power of LISP is automatically available
; as an extension to whatever you want to do
