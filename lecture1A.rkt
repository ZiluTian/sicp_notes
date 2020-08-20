#lang racket

; Author: Zilu Tian
; Date: March 15, 2020

; Lecture 1A. Overview and Introduction to Lisp

;Methods of Managing Complexity 
;- Black-box abstraction 
;- Conventional Interfaces
;- Metalinguistic Abstraction
;
;Learning a new language 
;- Primitives
;- Means of Composition
;- Means of Abstraction

(require "common.rkt")

; Heron's square-root method
;To find an approximation to \sqrt(x) 
;- Make a guess G
;- Improve the guess by averaging G and x/G
;- Keep improving the guess until it is good enough


(define square-root
  (λ (x)
    (define improve
      (λ (guess x)
        (average guess (/ x guess))))
    (define good-enough?
      (λ (f guess value)
        (let ([tolerance 0.001])
          (if (< (abs (- (f guess) value)) tolerance)
              true
              false))))
    (define try
      (λ (guess)
        (if (good-enough? square guess x)
            guess
            (try (improve guess x)))))
    (try 1)))
    
(square-root 9)


