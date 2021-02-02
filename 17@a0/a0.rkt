#lang racket

;; Assignment 0: complete three simple functions

(provide implies-value
         median-of-three
         point-distance)


; Compute the truth value of the proposition "x --> y" where x and y are booleans
(define (implies-value x y)
  (implies x y))

; Compute the median of three integers
(define (median-of-three x y z)
  ; (1 2 3) (3 2 1) (
  ; median y
  (if (and(< x y) (< y z)) 
           y
           (if (and(< z y) (< y x))
               y
               (if (and(< z x) (> z y))
                   y
  ; median x
                   (if (and(< y x) (< x z))
                       x
                       (if (and(< x y) (< z x))
                           x
                           (if (and(< z y) (> x z))
                               x
  ; else median z
                               z)))))))


; Code that didn't work is below...

 ; (cond
    ; median y
    ; [(< x y) (< y z) (> z x) (> y x) (> z y) y]
    ; median x
    ; [(< y x) (> z x) (> z y) (> x y) (< x z) x]
    ; median z
    ; [(< x z) (< z y) (> y x) (> z x) (> y z) z]))

  ; (if (< x y) (< y z) (> z x) (> y x) (> z y)
      ; y
      ; (if (< y x) (> z x) (> z y) (> x y) (< x z)
          ; x
          ; z)))

  ; (> z x)(> y x)(> z y) 
  ; (if (< x y) (< y z)
      ; (displayln y))
  
  ; median z
  ; (cond
    ; [(< x z) (< z y) (> y x) (displayln z)])
  ; (if (< x z) (< z y)
      ; (displayln z))
          
  ; median x
  ; (cond
    ; [(< y x) (< x z) (> z y) (displayln x)]))
  ; (if (< y x) (< x z)
      ; (displayln x)))       
          

; Compute the distance between two (x,y) pairs of integers
; NOTE: d(P, Q) = √ (x2 - x1)^2 + (y2 - y1)^2 
(define (point-distance x0 y0 x1 y1)
  ; (define (sub-x1-x0)
   ; (- (x1 x0)))
    
  ; (define (sub-y1-y0)
    ; (- (y1 y0)))
  ; (* (sub-y1-y0 sub-y1-y0))
  ; (* (sub-x1-x0 sub-x1-x0))
  
  ; (define (sum-x-y)
    ; (+ (sub-x1-x0) (sub-y1-y0)))
  
  ;TODO - try * itself instead of sqr
  
  ; (sqrt (sum-x-y)))

  (sqrt (+ (sqr (- x1 x0)) (sqr (- y1 y0)))))




