#lang racket

(require "common.rkt")
 
; Author: Zilu Tian
; Date: March 22, 2020

; Streams, Part 1

; Challenge with Assignment: Time, Identity, Object, Sharing

;(cons-stream x y)
;(head s)
;(tail s)
;the-empty-stream

; Establish conventional interfaces that can glue things together
; map, filter, accumulator 
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s)
             (map-stream proc (tail s))))))

(define (filter pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter pred
                          (tail s))))
    (else (filter pred (tail s)))))

(define (accumulate combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree (left-branch tree))
       (enumerate-tree (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1) (append-streams (tail s1) s2))))

(define (enum-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enum-interval (inc low) high))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd (enumerate-tree tree)))))

(define (old-fibs n)
  (accumulate cons '() (filter odd (map fib (enum-interval 1 n)))))

(define (flatten st-of-st)
  (accumulate append-streams the-empty-stream st-of-st))

(define (flatmap f s)
  (flatten (map f s)))

; Given N, find all pairs 0 < J < I <= N s.t. I+J is prime
(define find-prime-pairs
  (flatmap
   (λ (i)
     (map
      (λ (j) (list i j))
      (enum-interval 1 (dec i))))
   (enum-interval 1 n)))


;(cons-stream x y)
;Abbreviation for (cons x (delay y))
;(head s) -> (car s)
;(tail s) -> (force (cdr s))
;
;delay: take an expression, and return the promoise for doing the computation upon request
;force: take the promise and compute it
;
;(head (tail (filter prime? (e-i 10000 1000000))))
;-> ; can use substition model as there is no side effect and state 
;(head (tail (filter? prime? (cons 10000 (delay (e-1 10001 1000000))))))
;
;(delay <exp>)
;Abbreviation for (λ()<exp>)
;:: decouple the apparent order of the events in the program from the actual order of
;events in the computer  
;(force p) = (p)
;
;issue:
;Result in many tail computation, inefficient comparing to a simple list solution 
;(tail (tail (tail (tail ...))))
;
;solution:
;(delay <exp>)
;Abbreviation for (MEMO-PROC (λ () <exp> ))
;(MEMO-PROC) is to transform a procedure that only does computation once.
;When you call it for the first time, it will run the original procedure and remember its
;result. After that it won't do computation and just lookup

; proc: a procedure takes no arg
; memoization 
(define (memo-proc proc)
  (let ((already-run? null) (result null))
    (λ ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? (not null))
            result)
          result))))


(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (dec n) (tail s))))

(define (integers-from n)
  (cons-stream
   n
   (integers-from (+ n 1))))

; a stream of all integers 
(define integers
  (integers-from 1))

; Sieve of Eratosthenes
; list all the integers (2, 3, 4, ...). Start with 2. Check for primality
; Cross out all integers divisible by 2.
; Keep repeating it. 

(define (sieve s)
  (cons-stream
   (head s)
   (sieve (filter
           (λ (x)
             (not (divisible? x (head s))))
           (tail s)))))

(define primes
  (sieve (integers-from 2)))

; Programs that want to operate on all stream at once 
(define (add-streams s1 s2)
  (cond [(empty-stream? s1) s2]
        [(empty-stream? s2) s1]
        [else (cons-stream
               (+ (head s1) (head s2))
               (add-streams (tail s1)
                            (tail s2)))]))

(define (scale-stream c s)
  (map-stream (lambda (x) (* x c)) s))

; An inf stream of ones 
(define ones (cons-stream 1 ones))

(define integers-alt
  (cons-stream 1
    (add-streams integers-alt ones))) ; work because of 'delay

(define (integral s initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream dt s) int)))
  int)

; recursive data 
(define fib-stream
  (cons-stream 0
               (cons-stream 1
                            (add-streams fib-stream (tail fib-stream)))))

; Use delay to allow recursive definition
(define Y
  (integral (delay dy) 1 .001)) ; won't complain about dy since it is delayed

(define dy
  (map square y))

; Built explicit in the language: normal order evaluation (all delayed operations)
; applicative order evaluation (evaluate the operands before passing them)

; Cons of Normal Order Evaluation:
; - can't express iteration, all recursive ; dragging tails
; - Side-effect doesn't mix well with normal order evalution
;   (define x 0)
;   (define (id n) (set! x n) n)
;   (define (inc a) (+ a 1))
;
;   (define y (inc (id 3)))
;   x --> 0 ; lazily eval, contains previous value
;   y --> 4
;   x --> 3 ; (id 3) sets x

; Implement bank account using Stream rather than maintaining a local state
; message-passing object oriented implementation 
(define (make-deposit-account balance deposit-stream)
  (cons-stream balance (make-deposit-account
                        (+ balance (head deposit-stream))
                        (tail deposit-stream))))




