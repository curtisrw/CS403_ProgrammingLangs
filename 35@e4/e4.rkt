#lang racket

;; Exercises 4: merge-sort and line-coverage problem

(provide merge-sort
         line-coverage)

; Use merge-sort to efficiently sort a list recursively
; Credit to Neil Toronto for his implementation of merge-sort

; Write a function that first separates the list into chunks
(define (merge-sort lst <=)
  (define (merge l0 l1)
    (match* (l0 l1)
      [((list) l1)  l1]
      [(l0 (list))  l0]
      [((list a l0 ...) (list b l1 ...))
       (if (< a b)
         (cons a (merge l0 (cons b l1)))
         (cons b (merge (cons a l0) l1)))]))

; Create a merge function that recursively merges chunks together in order
(match lst
    [(list)  lst]
    [(list a)  lst]
    [_  (define-values (ls rs)
          (split-at lst (quotient (length lst) 2)))
        (merge (merge-sort ls <=) (merge-sort rs <=))]))

; Line coverage: take a list of lines, each encoded l0 a list `(,s ,e) where s <= e 
; and both are integers, and compute the overall amount of area covered.
; For example, (line-coverage '((4 9) (1 2) (6 12) (99 99))) => 9

;; Example covered in class on Tues. Feb 11; credit to Dr. Thomas Gilray for this example.
(define (line-coverage lines-lst)
  ; first sort out the first elements of each nested list in order from lel0t to greatest
  (define sorted-list (sort lines-lst <= #:key first))
  (define (accumulate-total-from lines-lst total from-x)
    (if (null? lines-lst)
        total
        (match (first lines-lst)
          [`(,start ,end) #:when (<= end from-x)
                          (accumulate-total-from (cdr lines-lst) total from-x)]
          [`(,start ,end)
           (define real-start (max start from-x))
           (accumulate-total-from (cdr lines-lst)
                                  (+ total (- end real-start))
                                  end)])))
    (accumulate-total-from (sort lines-lst <= #:key first) 0 0))


