#lang racket

(syntax-e (first (syntax-e #'(lambda (x) x))))
(syntax-e (first (syntax-e #'(lambda (x) x))))