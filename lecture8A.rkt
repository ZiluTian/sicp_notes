#lang racket

; Zilu Tian
; April 3, 2020 
; Logic Programming, Part 1


; Merge two sorted lists
(define (merge x y)
  (cond
    [(null? x) y]
    [(null? y) x]
    [else
     (let [(a (car x)) (b (car y))]
       (if (< a b)
           (cons a
                 (merge (cdr x) y))
           (cons b
                 (merge x (cdr y)))))]))

Logic (similarly for the other half)

for all X, (X and () merge-to-form X)

if
  (CDR-X and Y merge-to-form Z)
and
  A < (car Y)
then
  ((cons A CDR-X) and Y
                  merge-to-form (cons A Z))

; Relation is not a function,
; Want to be able answer all following questions
; Each would demand a function
(1 3 5) and (2 4 9) merge-to-form ?
(1 3 5) and ?x merge-to-form (1 2 3 4 5 9)
?x and ?y merge-to-form (1 2 3 4 5 9)

; 2 Big Differences: (logic programming vs traditional)
;1. No more input/output
;=> Relations do not have directionality
;2. May not be a single answer
;=> consider Q3 above

; Use Logic to express what is true
; Use Logic to check what is true
; Use Logic to find out what is true

;Query Language
;
;Primitive:
;=> query
;
;Means of Combination:
;=> and, not, or, lisp-value 
;
;Means of Abstraction:
;=> rule
;(rule
; (bigshot ?x ?dept) ; Rule Conclusion 
; (and               ; Rule Body 
;  (job ?x (?dept . ?y))
;  (not (and (supervisor ?x ?x)
;            (job ?z (?dept . ?w))))))


;Apply a rule
;- Evaluate the rule body relative to an environment formed
; by unifying the rule conclusion with the given query
;
;Apply a procedure
;- Evaluate the procedure body relative to an environment
;formed by binding the procedure parameters to the arguments

; Rewrite the sorted list merge using Query language 
(rule (merge-to-form () ?y ?y))
(rule (merge-to-form ?y () ?y))
(rule
 (merge-to-form
  (?a . ?x) (?b . ?y) (?b . ?z)) ; if the result after merging starts with b
 (and (merge-to-form (?a . ?x) ?y ?z)
      (lisp-value > ?a ?b)))
(rule
 (merge-to-form
  (?a . ?x) (?b . ?y) (?a . ?z)) ; if the result after merging starts with b
 (and (merge-to-form ?x (?b . ?y) ?z)
      (lisp-value > ?b ?a)))

; Order dependent
; closed-world assumption: anything that not specified is not true
; e.g. connectivity

; NOT: unable to deduce from the rules, rather than not true 
