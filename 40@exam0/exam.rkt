#lang racket

;; Coding exam 0: complete the three problems below and submit by the end of class for full credit
;;                or by midnight tonight for 50% credit.

(provide odd-indices
         greatest-without-going-over
         interleave)

; Return all of the elements at odd indices (starting from 0) in lst
; (odd-indices '(A B C D E)) => '(B D)
; (odd-indices '(0 1 2)) => '(1)
(define (odd-indices lst)
  (if (or (null? lst)             ; if the list is empty 
          (null? (cdr lst)))      ; or the list has a single element
      '()                         ; then return the empty list
      (cons (cadr lst)            ; otherwise `cons` the second element
            (odd-indices (cddr lst))))) ; and recursively advance two elements

; Return the element in `lst` which is as close as possible to `n`
; without going over. If no such element exists, return 'none
(define (greatest-without-going-over lst n)
    (cond ((null? lst) 'none)
        ((= n 0) (first lst))
        (else (greatest-without-going-over (rest lst) (- n 1)))))
  

; Interleave a list of lists so the first element of each list comes first, then the second, etc...
; E.g., (interleave '(0 1) '(2 3)) => '(0 2 1 3)
; When these lists are not of homogenious length, drop shorter lists from being interleaved after they end:
; E.g., (interleave '(A B C D E) '(1 2 3) '(0)) => '(A 1 0 B 2 C 3 D E)

;;;;;;;;;;;;;;; THIS NEEDS WORK ;;;;;;;;;;;;;;;;;

(define (interleave . arg-lst)
  (append (first (rest arg-lst) (first arg-lst))))


  
;    (match (list . arg-lst)
;      [(list (cons x arg-lst) (cons y arg-lst)) (cons x (cons y (interleave . arg-lst)))]
;      [(list '() ys)                  ys]
;      [(list xs '())                  xs])))

