#lang racket

;; Exercises 3: power-set and inclusion-exclusion principle

(provide power-set
         total-rect-area)

(define (power-set st)
  (combinations (set->list st)))

; total-rect-area: takes a list of rectangles (defined in e2) and returns the total covered area
; Note: do not double-count area covered by >1 rectangles
; E.g., (total-rect-area '((rect 0 0 2 2) (rect 1 1 3 3))) => 7
; Hint: use the power-set function and the inclusion-exclusion principle; review your functions from e2
(define (total-rect-area rect-list)
  (sum-qtree (foldl insert 'empty rect-list)))

(define (insert rect qt)
  (match rect
    [`(rect ,x0 ,y0 ,x1 ,y1) #:when (> (rect-area rect) 0)
                             (match qt
                               ['covered
                                'covered]
                               ['empty
                                `(qtree (,x0 ,y0) empty empty empty (qtree (,x1 ,y1) covered empty empty empty))]; maybe we can make it faster by placing points differently
                               [`(qtree ,pivot ,sw ,se ,nw ,ne)
                                `(qtree ,pivot
                                        ,(insert `(rect ,(min x0 (first pivot)) ,(min y0 (second pivot)) ,(min x1 (first pivot)) ,(min y1 (second pivot))) sw)
                                        ,(insert `(rect ,(max x0 (first pivot)) ,(min y0 (second pivot)) ,(max x1 (first pivot)) ,(min y1 (second pivot))) se)
                                        ,(insert `(rect ,(min x0 (first pivot)) ,(max y0 (second pivot)) ,(min x1 (first pivot)) ,(max y1 (second pivot))) nw)
                                        ,(insert `(rect ,(max x0 (first pivot)) ,(max y0 (second pivot)) ,(max x1 (first pivot)) ,(max y1 (second pivot))) ne)
                                        )]
                               )]
    [_ qt]))

(define (sum-qtree qt [wx0 -inf.0] [wy0 -inf.0] [wx1 +inf.0] [wy1 +inf.0])
  (match qt
    ['empty 0]
    ['covered (rect-area `(rect ,wx0 ,wy0 ,wx1 ,wy1))]
    [`(qtree ,pivot ,sw ,se ,nw ,ne)
     (+ (sum-qtree sw wx0 wy0 (first pivot) (second pivot))
        (sum-qtree se wx0 wy0 wx1 wy1)
        (sum-qtree nw wx0 wy0 wx1 wy1)
        (sum-qtree ne (first pivot) (second pivot) wx1 wy1))]
    ))

(define (rect-area rect)
  (match rect
    [`(rect ,x0 ,y0 ,x1 ,y1) (* (- x1 x0) (- y1 y0))]
    [_ 0]))

(define (insert-list rect-list)
  (foldl insert 'empty rect-list))


;#lang racket
;
;;; Exercises 3: power-set and inclusion-exclusion principle
;
;(provide power-set
;         total-rect-area)
;
;; power-set: takes an arbitrary Racket (set) and returns its power-set, the set of all its subsets
;(define (power-set st)
;  (if (set-empty? st)
;      (set)
;      (let ([ps (power-set (set-rest st))])
;        (append (set-map (λ (l) 
;                       (cons (set-first st) l)) 
;                     ps) 
;                ps))))
;
;(define (power-setL aL)
;  (if (empty? aL)
;      '(())
;      (let ((rst (power-setL (set-rest aL))))
;        (append (map (lambda (x) (cons (set-first aL) x))
;                     rst)
;                rst))))
;
; (foldr (lambda (e acc)
;           (let ((pws (power-set (set-rest (st)))))
;             (append (map (lambda (x) (cons e x))
;                          pws)
;                     pws)))
;         (st))
;
;
;
;; total-rect-area: takes a list of rectangles (defined in e2) and returns the total covered area
;; Note: do not double-count area covered by >1 rectangles
;; E.g., (total-rect-area '((rect 0 0 2 2) (rect 1 1 3 3))) => 7
;; Hint: use the power-set function and the inclusion-exclusion principle; review your functions from e2
;(define (total-rect-area rect-list)
;  (match (flatten rect-list)
;         [`(rect ,x0 ,y0 ,x1 ,y1) `(,x0 ,x1 ,y0 ,y1)
;               (<= x0 x1)
;               (<= y0 y1)]))
;  
;;    (define (current-list-pos)
;;      (car rect-list))
;;  
;;    (define (distance-x)
;;      (- (list-ref (current-list-pos) 2)  (list-ref (current-list-pos) 0)))
;;  
;;    (define (distance-y)
;;      (- (list-ref (current-list-pos) 3)  (list-ref (current-list-pos) 1)))
;;  
;; (* (distance-x) (distance-y)))
;
;;  (if (null? rect-list)
;;      '()
;;      (
;
;; (define (remove-rect set)
;;      (if (null? set)
;;      '()
;;      (flatten (cons (rest (first set)) (remove-rect (rest set))))))
;
;    
;
;;  (define (sum)
;;    (for ([i (length (remove-rect rect-list))])
;;       
;
;  ; (for ([i (length (remove-rect rect-list))])
;      
;
;;(map (lambda (number)
;;         (+ 1 number))
;;       (remove-rect rect-list)))
;
;
;
;  
;
;    ; (cons (first (current-list-pos)) (rest rect-list)))
;;      (if (empty? x)
;;        x
;;        (cons (first (current-list-pos)) (sum (rest rect-list))))))
; 
;;  (sum rect-list))
;
;;; (rect-area (rect 0 1 3 3))
;;
;  
;
;
