#lang racket

;; Exercises 2: rectangle library

(provide rect
         rect?
         rect-area
         rect-intersect
         rect-list-intersect
         all-sub-rectangles)

; any two opposing corners of a grid-aligned rectangle as pairs (x0,y0), (x1,y1)
; --> `(rect ,lower-left-x ,lower-left-y ,upper-right-x ,upper-right-y) 
(define (rect x0 y0 x1 y1)
  ; return a normalized rect-tagged s-expr representation of the rectangle

  (define (lower-left-x)
   (if (< x0 x1)
       x0
       x1))

  (define (lower-left-y)
    (if (< y0 y1)
       y0
       y1))

  (define (upper-right-x)
    (if (< x0 x1)
       x1
       x0))

  (define (upper-right-y)
    (if (< y0 y1)
       y1
       y0))
  
`(rect ,(lower-left-x) ,(lower-left-y) ,(upper-right-x) ,(upper-right-y)))


; Predicate defining a rectangle
(define (rect? r)
  (match r
         [`(rect ,x0 ,y0 ,x1 ,y1)
          (and (andmap integer? `(,x0 ,x1 ,y0 ,y1))
               (<= x0 x1)
               (<= y0 y1))]
         [else #f]))

; Given a rect?, yield its (integer?) area
(define (rect-area rect)
; (rect-area (rect 0 1 3 3))

  (define (normalize-list)
    (remove (car rect) rect))
    
  (define (distance-x)
    (- (list-ref (normalize-list) 2)  (list-ref (normalize-list) 0)))
  (define (distance-y)
    (- (list-ref (normalize-list) 3)  (list-ref (normalize-list) 1)))
  (* (distance-x) (distance-y)))
  

; Compute the rectangular intersection of any two rectangles
; If there is no intersection, return a rectangle with 0 area.
(define (rect-intersect rectA rectB)
  ; (rect-intersect (rect 0 8 8 1) (rect 3 5 5 12))
  (rect (max (match (cdr rectA) [(list x0 _ _ _) x0]) (match  (cdr rectB) [(list x0 _ _ _) x0]))
        (min (match (cdr rectA) [(list _ y0 _ _) y0]) (match  (cdr rectB) [(list _ y0 _ _) y0]))
        (min(match (cdr rectA) [(list _ _ x1 _) x1]) (match  (cdr rectB) [(list _ _ x1 _) x1]))
        (min (match (cdr rectA) [(list _ y0 _ _) y0]) (match  (cdr rectB) [(list _ _ _ y1) y1]))
  ))

; Compute the intersection of a list of one or more rectangles
; E.g., the list `((rect 0 0 10 10) (rect 0 -5 10 1) (rect -5 -5 2 5))
;       has intersection `(rect 0 0 2 1)
(define (rect-list-intersect rect-list)
  'todo)

; Compute a Racket (set) of all sub-rectangles in the given rectangle
; We will call any rectangle r', with integer side-lengths of at least 1, a "sub-rectangle" of r iff r fully contains r'
; E.g., (all-sub-rectangles (rect 0 0 0 0)) => (set)
; E.g., (all-sub-rectangles (rect 0 0 1 1)) => (set `(rect 0 0 1 1))
; E.g., (all--sub-rectangles (rect 10 5 11 7)) => (set `(rect 10 5 11 7) `(rect 10 5 11 6) `(rect 10 6 11 7))
; Hint: can you solve this using the `foldl` and `range` functions?
(define (all-sub-rectangles r)
  'todo)


