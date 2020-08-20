#lang racket

; Author: Zilu Tian
; Date: March 19, 2020

;(provide evaluate)

(require "common.rkt")
(require (only-in r5rs scheme-report-environment))

; Pattern Matching and Rule-based Substitution 

; Each rule has a pattern and a skeleton
; Have a matcher and instantiater

; pattern  --rule-->  skeleton
;   |                    |
; match             instantiation
;(matcher)          (instantiater)
;   |                    |
; source              target
;expression  ----->  expression
;
;Instead of bringing rules to the level of the computer by writing a program
;that are rules in computer's language, we want to bring the computer to the
;level of us, by writing a way that computer can understand rules of this kind
;
;               pattern
;                  |
;                  v
;expression --> matcher --> dictionary 
;                  ^
;                  |
;              dictionary 
;
;
;dictionary, skeleton --> instantiater --> expression
                  

; Pattern Match
; foo - match exactly foo
; (f a b) - match a list where 1st is f, 2nd is a and 3rd is b
; (? x) - match anything, call it x
; (?c x) - match any constant, call it x 
; 
; Skeleton Instantiation
; foo - instantiates itself
; (f a b) - instantiates to a 3-list result of instantiating each of f,a,b
; (: x) - instantiates to the value of x as in the matched pattern 

(define user-initial-environment (scheme-report-environment 5))

; Dictionary
(define variable-name cadr)

(define (empty-dictionary) '())

(define lookup
  (λ (var dict)
    (let [(v (assq var dict))]
      (if (not v) var (cadr v)))))

(define extend-dict
  (λ (pat dat dict)
    (let [(name (variable-name pat))]
      (let [(v (assq name dict))]
        (cond [(not v) (cons (list name dat) dict)]
              [(eq? (cadr v) dat) dict]
              [else 'failed])))))

(define atomic?
  (λ (expr)
    (not (pair? expr))))

(define constant? number?)
(define variable? symbol?)

;(define compound? pair?)
(define compound? (λ (x) (not (atomic? x))))

(define arbitrary-constant?
  (λ (pat)
    (and (pair? pat) (eq? (car pat) '?c))))

(define arbitrary-variable?
  (λ (pat)
    (and (pair? pat) (eq? (car pat) '?v))))

(define arbitrary-expression?
  (λ (pat)
    (and (pair? pat) (eq? (car pat) '?))))

(define match
  (λ (pat exp dict)
    (cond [(eq? dict 'failed) 'failed]
          [(atomic? pat) (if (atomic? exp)
                             (if (eq? pat exp)
                                 dict
                                 'failed)
                             'failed)]
          ; [(atomic? exp) 'failed]
          ; pattern variables
          [(arbitrary-constant? pat) ; ?c 
           (if (constant? exp)
               (extend-dict pat exp dict)
               'failed)]
          [(arbitrary-variable? pat) ; ?v 
           (if (variable? exp)
               (extend-dict pat exp dict)
               'failed)]
          [(arbitrary-expression? pat) ; ? x arbitrary expression 
           (extend-dict pat exp dict)]
          [else (match (cdr pat) (cdr exp) (match (car pat) (car exp) dict))])))

(test-case
 "Check match"
 (check-equal? (match '(?c c) '10.5 (empty-dictionary)) '((c 10.5)))
 (check-equal? (match '(?v v) 'x (empty-dictionary)) '((v x)))
 (check-equal? (match '(?c c) 'x (empty-dictionary)) 'failed)
 (check-equal? (match '(? x) '(+ 10 5) (empty-dictionary)) '((x (+ 10 5))))
 (check-equal? (match '(dd) '(dd) (empty-dictionary)) '())
 (check-equal? (match '(dd (?c c) (? v)) '(dd 10 x) (empty-dictionary)) '((v x) (c 10)))
 (check-equal? (match '(dd (+ (? x1) (? x2)) (? v)) '(dd (+ x y) x) (empty-dictionary)) '((v x) (x2 y) (x1 x)))
 (check-equal? (match '(* (* (? e1) (?c e2)) (? e3)) '(* (* x 4) y) (empty-dictionary)) '((e3 y) (e2 4) (e1 x)))
 "Test cases for match passed")

(define evaluate
  (λ (form dict)
    (if (atomic? form)
        (lookup form dict)
        (apply (eval (lookup (car form) dict)
                     user-initial-environment)
               (map (λ (v) (lookup v dict))
                       (cdr form))))))

(test-case
 "Check evaluate"
 (check-equal? (evaluate '(+ x y) '((x 1) (y 3))) 4)
 (check-equal? (evaluate '(* 32 5) '()) 160)
 (check-equal? (evaluate '(mul 32 5) '((mul (lambda (x y) (+ (* x 5) y))))) 165)
 "Test cases for evaluate passed")

(define instantiate
  (λ (skeleton dict)
    (define loop
      (λ (s)
        (cond [(atomic? s) s]
              [(skeleton-evaluation? s) (evaluate (eval-exp s) dict)]
              [else (cons (loop (car s)) (loop (cdr s)))])))
    (loop skeleton)))

(define skeleton-evaluation?
  (λ (s)
    (and (pair? s) (eq? (car s) ':))))

(define eval-exp cadr)

(define pattern car)
(define skeleton cadr)

(test-case
 "Check instantiate"
 (check-equal? (instantiate 0 (empty-dictionary)) 0)
 (check-equal? (instantiate '(: e) '((e (+ a b)))) '(+ a b))
 (check-equal? (instantiate '(: (op e1 e2)) '((op +) (e1 14) (e2 43))) 57) 
 (check-equal? (instantiate '(* (: (* e1 e2)) (: e3)) '((e1 20) (e2 35) (e3 (+ x y)))) '(* 700 (+ x y)))
 "Test cases for instantiate passed")

(define simplifier
  (λ (the-rules)
    (define (simplify-exp exp)
      (try-rules (if (compound? exp) (map simplify-exp exp) exp)))
    (define (try-rules exp)
      (define (scan rules)
        (if (null? rules)
            exp
            (let [(dict (match (pattern (car rules))
                          exp
                          (empty-dictionary)))]
              (if (eq? dict 'failed)
                  (scan (cdr rules))
                  (simplify-exp
                   (instantiate
                       (skeleton (car rules))
                     dict))))))
      (scan the-rules))
    simplify-exp))


(define deriv-rules
  '(
    (  (dd (?c c) (? v)) 0) ; derivative of a constant c w.r.t a variable v 
    (  (dd (?v v) (? v)) 1)
    (  (dd (?v u) (? v)) 0)

    (  (dd (+ (? x1) (? x2)) (? v))
       (+ (dd (: x1) (: v))
          (dd (: x2) (: v))))

    (  (dd (* (? x1) (? x2)) (? v))
       (+ (* (: x1) (dd (: x2) (: v)))
          (* (dd (: x2) (: v)) (: x2))))

    (  (dd (** (? x) (?c n)) (? v))
       (* (* (: n)
             (** (: x)(: (- n 1))))
          (dd (: x)(: v))))))

(define algebra-rules
  '(
    (  ((? op) (?c e1) (?c e2))
       (: (op e1 e2)))
    (  ((? op) (? e1) (?c e2))
       ((: op) (: e2) (: e1)))
    (  (+ 0 (? e)) (: e))
    (  (* 1 (? e)) (: e))
    (  (* 0 (? e)) 0)
    (  (* (?c e1) (* (?c e2) (? e3)))
       (* (: (* e1 e2)) (: e3)))
    (  (* (? e1) (* (?c e2) (? e3)))
       (* (: e1) (* (: e2) (: e3))))
    (  (* (* (? e1) (?c e2)) (? e3))
       (* (: e2) (* (: e2) (: e3))))
    (  (+ (?c e1) (+ (?c e2) (? e3)))
       (+ (: (+ e1 e2)) (: e3)))))

(test-case
 "Check pattern and skeleton"
 (check-equal? (pattern (car deriv-rules)) '(dd (?c c) (? v)))
 (check-equal? (skeleton (car deriv-rules)) '0))

;((simplifier deriv-rules) '(dd (+ x y) x))
;((simplifier deriv-rules) '(dd y x))

   
    




    