#lang racket

;; Exercises 0: complete three simple functions

(provide nor-value
         least-of-three
         hypotenuse-length)


; Compute the truth value of the proposition "~(x \/ y)" (i.e., "not of (x or y)", or "x nor y") where x and y are booleans
(define (nor-value x y)
  (if (not(or x y))
      #t
      #f))
  
; Compute the least of three integers
(define (least-of-three x y z)
  (min x (min y z)))

; Compute the length of the Hypotenuse of a triangle with perpendicular edges of length a and b 
(define (hypotenuse-length a b)
  (define (sum-a-b)
    (+ (* a a) (* b b)))
  (sqrt (sum-a-b)))

  ;(define (sqr-a a)
   ; (* a a)
  ;(define (sqr-b b)
   ; (* b b)
  ;(define (sum-sqr)
   ; (+ a b)
  ;(sqrt sum-sqr)))


