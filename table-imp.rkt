#lang racket
(require compatibility/mlist)
(require rackunit)

(provide (rename-out [lookup2 get][insert2 put]) make-table)

; Author: Zilu Tian
; Date: March 20, 2020

; table
; https://web.mit.edu/alexmv/6.037/sicp.pdf, P.362


; 1D table represented as a headed list 
(define (lookup key table)
  (let [(record (assoc key (mcdr table)))]
    (if record ; check the record is not empty 
        (mcdr record)
        false)))

(define (assoc key records)
  (cond [(null? records) false]
        [(equal? key (mcar (mcar records))) (mcar records)]
        [else (assoc key (mcdr records))]))

(define (insert! key value table)
  (let [(record (assoc key (mcdr table)))]
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                  (mcons (mcons key value)
                         (mcdr table))))))

(define (make-table)
  (mlist '*table*))

; test 
(test-case
 "Check 1D table"
 (define A (make-table))
 (insert! 'add '+ A)
 (check-equal? (mcdr A) (mcons (mcons 'add '+) '())) ; table content
 (check-equal? (mcar A) '*table*)
 (check-equal? (lookup 'add A) '+)         ; lookup 
 (check-equal? (lookup 'minus A) #f)       ; non-existent 
 (insert! 'add '+C A)   ; overwrite
 (check-equal? (lookup 'add A) '+C))       ; lookup overwrite 


; 2D table

(define (lookup2 key1 key2 table)
   (let [(subtable (assoc key1 (mcdr table)))]
    (if subtable ; check the subtable is not empty
        (lookup key2 subtable)
        false)))

(define (insert2 key1 key2 value table)
  (let [(subtable (assoc key1 (mcdr table)))]
    (if subtable
        (insert! key2 value subtable)
        (set-mcdr! table
                   (mcons (mlist key1 (mcons key2 value))
                          (mcdr table))))))

(test-case
 "Check 2D table"
 (define B (make-table))
 (insert2 'add 'complex '+C B)     ; insert
 (check-equal? (lookup2 'add 'complex B) '+C)         ; lookup 
 (check-equal? (lookup2 'minus 'complex B) #f)        ; non-existent 
 (insert2 'add 'real '+R B)       ; overwrite
 (check-equal? (lookup2 'add 'real B) '+R)
 (check-equal? (lookup2 'add 'complex B) '+C))        ; lookup overwrite               