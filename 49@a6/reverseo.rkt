#lang racket

(provide reverseo)
(provide appendo)
(require "mk.rkt")

;; Example: an appendo function for relational list append
(define (appendo lst0 lst1 full)
  (conde ; conde is disjunction: either case in square brackets must be true
   [(== lst0 '())
    ; this case says: lst0 is unified (matched) with '(),
    ; and full is unified with lst1
    (== full lst1)]
   [(fresh (first rest tail)
           ; we introduce three new fresh (initially unconstrained variables)
           ; this case says: lst0 deomposes into (cons first rest),
           (== lst0 (cons first rest))
           ; full decomposes into (const first tail)
           (== full (cons first tail))
           ; and rest appended to lst1 matches tail
           (appendo rest lst1 tail))]))


; Your task is to write a (list) reversing relation: 
(define (reverseo lst0 lst1)
  ; Watching Will and Dan's video helped me with this
  (conde
   [(== lst0 '())  ; lst0 must be matched with '(), or
    (== lst1 '())] ; lst1 must be matched with '()
   [(fresh (first rest tail) ; three fresh variables 
           (== lst0 (cons first rest)) ; lst0 becomes (cons first rest)
           (appendo tail (list first) lst1) ; appending tail, first, lst1
           (reverseo rest tail))])) ; --> '(tail rest)

