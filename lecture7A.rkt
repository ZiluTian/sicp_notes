#lang racket

; Zilu Tian
; March 24, 2020 
; Metacircular Evaluator, Part 1


(define operator car) ; (+ 1 5)
(define operands cdr)
(define quote-sym cadr) ; 'foo => (quote foo) -> foo
(define cond-clauses cdr) ; (cond (p1 e1) (p2 e2) ...)

(define eval-ow
  (λ (exp env)
    (cond
      ; special cases
      [(number? exp) exp] 
      [(symbol? exp) (lookup exp env)] ; x -> 3; car -> #[procedure]
      [(eq? (operator exp) 'quote) (quote-sym exp)] 
      [(eq? (operator exp) 'lambda) (list 'closure (cdr exp) env)]; (λ(x)(+ x y)) -> (closure ((x)(+ x y)) <env>) ; Need to distinguish 
      [(eq? (operator exp) 'cond)
       (evcond (cond-clauses exp) env)] 
      ; default combination 
      [else (apply-ow (eval-ow (operator exp) env)
                      ((operands exp) env))])))

; apply's job is to take a procedure and apply it to its args after both have been eval
; (list 'closure '((x)(λ (y) (x + y))) '<e0>)
(define proc-type car) ; 'closure 
(define proc-body cadadr) ; '(λ (y) (x + y))
(define proc-arg caadr); '(x)
(define proc-env caddr); '<e0>

(define apply-primop apply);

(define apply-ow
  (λ (proc args)
    (cond [(primitive? proc) (apply-primop proc args)]
          [(eq? (proc-type proc) 'closure)
           (eval (proc-body proc) (bind proc-arg proc) args (proc-env proc))] ; compound made up by lambda 
          [else error]))) 

; l: list of operands
(define first-operand car)

(define evlist
  (λ (l env)
    (cond [(eq? l '()) '()]
          [else (cons (eval-ow (first-operand l) env) ; evaluate the operand
                      (evlist (cdr l) env))])))

;(eval '(((λ(x) (λ(y)(+ x y))) 3) 4) <e0>)
;->
;(apply (eval '((λ(x)(λ(y)(+ x y))) 3) <e0>)
;       (evlist '(4) <e0>))
;->
;(apply (eval '((λ(x)(λ(y)(+ x y))) 3) <e0>)
;       (cons (eval '(4) <e0>)
;             (evlist '() <e0>)))
;->
;(apply (eval '((λ(x)(λ(y)(+ x y))) 3) <e0>)
;       (cons 4 '()))
;->
;(apply (apply (eval '(λ(x)(λ(y)(+ x y))) <e0>)
;              '(3))
;       '(4))
;->
;(apply (apply '(closure ((x)(λ(y)(+ x y))) <e0>)
;              '(3))
;       '(4))
;->
;(apply (eval '(λ(y)(+ x y)) <e1>)
;       '(4))
;->
;(apply '(closure((y)(+ x y)) <e1>)
;       '(4))
;->
;(eval '(+ x y) <e2>)
;->
;(apply (eval '+ <e2>)
;       (evlist '(x y) <e2>))
;->
;(apply 'primop-addition '(3 4))
;-> 
;7 

; cond (list '[(> 0 n) (do_a)] '[(<5 n) (do_b)] '[else (do_c)])
(define first-test-expr caar) ; (> 0 n)
(define first-then-body cadar) ; (do_a)
(define rest-clauses cdr) ; [(<5 n) (do_b)] [else (do_c)]

(define evcond
  (λ (clauses env)
    (cond [(eq? clauses '()) '()]
          [(eq? (first-test-expr clauses) 'else)
           (eval (first-then-body clauses) env)]
          [(false? (eval (first-test-expr clauses) env))
           (evcond (next-clause clauses) env)]
          [else
           (eval (rest-clauses clauses) env)])))

(define bind
  (λ (vars vals env)
    (cons (pair-up vars vals) env)))

(define pair-up
  (λ (vars vals)
    (cond [(eq? vars '())
           (cond [(eq? vals '()) '()]
                 [else (error TMA)])]
          [(eq? vals '()) (error TFA)]
          [else (cons (cons (car vars)
                            (car vals))
                      (pair-up (cdr vars) (cdr vals)))])))

(define lookup
  (λ (sym env)
    (cond [(eq? env '()) (error UBV)]
          [else ((λ (vcell)
                   (cond [(eq? vcell '())
                          (lookup sym (cdr env))]
                         [else (cdr vcell)]))
                 (assq sym (car env)))])))

(define assq
  (λ (sym alist) ; symbol and a list of pairs
    (cond [(eq? alist '()) '()]
          [(eq? sym (caar alist))
           (car alist)]
          [else (assq sym (cdr alist))])))
          


          