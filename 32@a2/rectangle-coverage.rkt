#lang racket

;; Assignment 2: Quad-trees and immutable data-structures

(provide total-rect-area)

; total-rect-area: returns the total integer area covered by any (one or more) rectangles in the given list 
; Don't double-count. Should be identical to the same solution from e3.rkt, except this version must be in O(n log n)
; Hint: implement an immutable quad-tree to represent 2D space; FYI, a solution takes only ~30-40 lines of code.

;; ****************** THIS STILL NEEDS WORK ******************

(define (total-rect-area rect-list)
; (total-rect-area '((rect 1 1 4 4)))
; (total-rect-area '((rect 0 0 2 2) (rect 1 1 3 3)))
; (remove (car (list-ref rect-list 0)) (rect-list)))
  
;;;;; rect-function ;;;;;
  
; break rect-list down by getting the first element in rect-list
; complete the math for that element (area for that rect)
; add that area (number) to a new list
; recur

;;;;;

;  (define (current-list-pos)
;    (first rect-list))
;
;  
;  (define (flatten-lst)
;    (flatten rect-list))
;  (remove `rect (flatten-lst)))
;  
;  
;  (define (sum-rect-area lon)
;    
;    (define (current-list-pos)
;      
;      (first lon))
;    
;    (define (distance-x)
;      (- (list-ref (current-list-pos) 3)  (list-ref (current-list-pos) 1)))
;    
;    (define (distance-y)
;      (- (list-ref (current-list-pos) 4)  (list-ref (current-list-pos) 2)))
;    
;    (define (rect-area)
;      (* (distance-x) (distance-y)))
;    
;    (for/list ([e lon])
;      (sum-rect-area e))
;    )
;  (sum-rect-area rect-list))
 


  (define (lt-add line lt)
   (match rect-list
     [`(,lower-x ,lower-y ,upper-x ,upper-y) #:when (and (< lower-x upper-x) (< lower-y upper-y))
     (match lt
       ['covered
        'covered]
       ['empty
        `(lt lower-x empty (lt ,lower-y covered empty))]
       [`(lt ,pivot ,left ,right)
        `(lt ,pivot
             ,(lt-add `(,(lower-x pivot) ,(upper-x pivot)) left)
             ,(lt-add `(,(lower-y pivot) ,(max upper-y pivot)) right))])]
     [_ lt]))
  
 (define (sum-rects re wstart wend)
   (match re
     ['empty 0]
     ['covered (-  wstart)]
     [`(lt ,pivot ,left ,right)
      (+ (sum-rects left wstart pivot)
         (sum-rects right pivot wend))]))
  (sum-rects (foldl lt-add 'empty rect-list)))
