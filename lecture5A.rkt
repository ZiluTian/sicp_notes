#lang racket
(require "common.rkt")

; Author: Zilu Tian
; Date: March 20, 2020

; Assignment, State, and Side-effects 

; processes evolved by functional programs can be understood by substitution
; function: 1-1 mapping
; substitution model no longer applicable: need to represent name as referring to places

(define count 1)
(define (demo x)
  (set! count (inc count))
  (+ x count))

(test-case
 "Check for demo"
 (check-equal? (demo 3) 5)
 (check-equal? (demo 3) 6)
 "Test cases for demo (non-function) passed")

; functional version 
(define (fact-func n)
  (define (iter m i)
    (cond [(> i n) m]
          [else (iter (* i m)(+ i 1))]))
  (iter 1 1))

;let, define, set!
;
;(let [(var1 e1) (var2 e2)] e3)
;<=>
;((λ (var1 var2) e3) e1 e2)
;
;define is a syntatic sugar whereby many variables created by let and setup once
;
;let and define introduce sth once and stay for the scope
;
;set! changes the previous value of a variable to another


;Environment Model
;- "bound": a variable v is bound in an expression E if the meaning of E is unchanged
;  by the uniform replacement of a variable W, not occurring in E, for every occurrance
;  of V in E.
;  (e.g. \forall x, \exists y p(x,y)  => x, y are bound  (can replace w/ any vars)
;        (integral (/ 1 (+ 1 (square x))) x 0 1)  => x is bound
;        (λ (y) ((λ (x) (* x y)) 3)) => x, y are bound
;        (λ (y) (+ x y)) => x is not bound (free))
;- "free": a variable V is free in an expression E if the meaning of E is changed by the
;  uniform replacement of a variable W, not occurring in E, for every occurance of V
;  in E.
;  (e.g. (λ (y) ((λ (x) (* x y)) 3)) => * is free)
;- "scope": If X is a bound variable in E, then there is a lambda expression where it's bound.
;  We call the list of formal parameters of the lambda expression the "bound variable list"
;  and we say that the lambda expression "binds" the variable "declared" in its bound
;  variable list. In addition, those parts of the expressions where a variable has a value
;  defined by the lambda expression which binds it is called the "scope" of the variable.
;  (e.g. (λ (y) ((λ (x) (* x y)) 3)) => scope of x: (* x y), scope of y: ((λ (x) (* x y)) 3))
;- "environments"
;  Environmentment is a way of doing substitution virtually
;  - Represent a place where something is stored which is the substitution you haven't done
;  - A place where everything accumulates, where names of the variables are associated with
;    values they have
;  - "frames" are pieces of environment and are chained together by parent links


;A, B, C, D are environments
;- A is an environment consists of table labelled frame II followed by the table
;  labelled frame I
;  x = 7 in frame II shadows x = 3 in frame I
;- frames correspond to applications of procedures 
;
;C and D are the same environments
;I, II, III are frames
;z and x are bound in II
;
;- Procedure 
;
;Evaluation Rules
;- Rule 1: How to apply a procedure to its arguments 
;  A procedures object is applied to a set of arguments by constructing a frame,
;  binding the formal parameters of the procedure to the actual arguments of the call,
;  and then evaluating the body of the procedure in the context of the new environment
;  constructed. The new frame has as its enclosing environment the environment part of 
;  procedure object being applied.
;  
;  B is the newly constructed environment for evaluating body of expression E
;
;- Rule 2: How to construct procedure
;  A lambda-expression is evaluated relative to a given environment as follows: a new
;  procedure object is formed, combining the text (code) of the lambda-expression with a
;  pointer to the environment of evaluation 
;
;  
;  the environment pointer captures the place where the lambda expression was evaluated,
;  where the definition was used to make a procedure

; imperative version
(define (fact n)
  (let [(i 1) (m 1)]
    (define (loop)
      (cond [(> i n) m]
            [else
             (set! m (* i m))
             (set! i (+ i 1))
             (loop)]))
    (loop)))

(test-case
 "Check for fact-func"
 (check-equal? (fact-func 3) 6)
 "Test cases for fact-func passed")

(define make-counter
  (λ (n)
    (λ ()
      (set! n (inc n))
      n)))

; Computational objects, two counters with independent local state
(define c1 (make-counter 0))
(define c2 (make-counter 10))

(test-case
 "Check make-counter example"
 (check-equal? (c1) 1)
 (check-equal? (c2) 11)
 (check-equal? (c1) 2)
 (check-equal? (c2) 12)
 "Test cases for make-counter example passed")


; define: a way of changing the environment (add to it)

; make an environment by applying make-counter to 0
; evaluate the body of the expression in the new environment
; evaluate a lambda expression will make a procedure object
; the procedure object has name c1 in global env


;Actions and Identity
;
;We say that an action A had an effect on an object X (X was changed by A)
;if some property p which was true of X becore A became false of X after A.
;
;We say that two objects, X and Y, are the same if any action which has an
;effect on X has the same effect on Y. 



; Cesaro's method for estimating Pi

(define (rand)
  (random 1 4294967087))

(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

; rand is not a function: expects different output 
(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond [(= remaining 0)
           (/ passed trials)]
          [(experiment) (iter (dec remaining) (inc passed))]
          [else (iter (dec remaining) passed)]))
  (iter trials 0))

;(define rand
;  (let ((x random-init)) ; hidden local state
;    (λ ()
;      (set! x (rand-update x))
;      x)))