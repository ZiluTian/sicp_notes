#lang racket

; The picture language
; Author: Zilu Tian
; Date: March 19, 2020

;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(require (planet soegaard/sicp:2:1/sicp))

;(paint (number->painter 0))

(paint (procedure-painter (lambda (x y) (* 255 x y))))




