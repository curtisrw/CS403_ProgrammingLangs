#lang racket
(require racket/trace)
;; Exercises 6: Reducing (normalizing/simplifying) lambda calc terms by textual substitution
;;              using four different orders of evaluation (applicative, normal, CBV, CBN)
(provide collect-evaluation-trace
         applicative-order-reduce
         normal-order-reduce
         CBV-order-reduce
         CBN-order-reduce)

; A predicate for terms in the lambda calculus
(define (exp? e)
  (match e
         [(? symbol?) #t]
         [`(,(? exp?) ,(? exp?)) #t]
         [`(lambda (,(? symbol?)) ,(? exp?)) #t]
         [_ #f]))

; Free variables (may be useful for defining capt-avoid-subst)
(define (free-vars exp)
  (match exp
         [(? symbol?) (set exp)]
         [`(,e0 ,e1) (set-union (free-vars e0) (free-vars e1))]
         [`(lambda (,x) ,e0) (set-remove (free-vars e0) x)]))

; Capture avoiding substitution
(define (capt-avoid-subst e0 x e1)
  (call/cc (lambda (ret)
             (define e1free (free-vars e1))
             (define (subst e)
               (match e
                 [(? symbol? y) #:when (equal? x y)
                     e1]
                 [(? symbol? y) y]
                 [`(lambda (,y) ,body) #:when (set-member?  e1free y) (ret 'failed)]
                 [`(lambda (,y) ,body) #:when (not (equal? x y))
                                       `(lambda (,y) ,(subst body))]
                 [`(lambda (,y) ,body)
                  `(lambda (,y) ,body)]
                 [`(,e0 ,e1)
                  (map subst e)]))
             (subst e0))))

;;;;;;;;;;;;;;;;;;;;; NOTES FROM LAB ;;;;;;;;;;;;;;;;;;;;;

; ((lambda (x) (* x 2) (lambda (y) (+ y y) 2)))
;     (* 4 2) = 8              (+ 2 2) = 4
; (* (+ 2 2) 2) (+ 2 2)
; (* (4) 2)
;    8


; ((lambda (m) ((lambda (u) (u u)) ((lambda (x) x) m))) ((lambda (a) a) (lambda (b) b)))
;                                                          ^^ this reduces to: (lambda (b) b)
;
; So, now we have the following: ((lambda (u) (u u)) ((lambda (x) x) (lambda (b) b))
; Reducing further: (lambda (u) (u u) (lambda (b) b))
; Again:            ((lambda (b) b) (lambda (b) b))
; Finally:          (lambda (b) b)



; OUTPUT


;(collect-evaluation-trace applicative-order-reduce '((lambda (m) ((lambda (u) (u u)) ((lambda (x) x) m))) ((lambda (a) a) (lambda (b) b))))
;'(((lambda (m) ((lambda (u) (u u)) ((lambda (x) x) m))) ((lambda (a) a) (lambda (b) b)))
;  ((lambda (m) ((lambda (u) (u u)) m)) ((lambda (a) a) (lambda (b) b)))
;  ((lambda (m) (m m)) ((lambda (a) a) (lambda (b) b)))
;  ((lambda (m) (m m)) (lambda (b) b))
;  ((lambda (b) b) (lambda (b) b))
;  (lambda (b) b))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Reduce the given expression by exactly one beta-reduction using
; applicative evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no applicative order redex.
; Skip over any redexes that cannot be reduced using capture avoiding subst.
(define (applicative-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (applicative-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
          (define body-st (applicative-order-reduce body))
          (if (set-empty? body-st)
              ; No redex under this lambda:
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; This is the innermost+leftmost redex
                    (let ([body+ (capt-avoid-subst body y ea)])
                      (if (eq? body+ 'failed)
                          (set) ; reducing redex failed
                          (set body+)))
                    ; A redex under the argument expression was found
                    (set `((lambda (,y) ,body) ,(set-first ea-st)))))
              ; A redex under the lambda was reduced already
              (set `((lambda (,y) ,(set-first body-st)) ,ea)))]
         [`(,ef ,ea)
          (define ef-st (applicative-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))
; (trace applicative-order-reduce)

; Reduce the given expression by exactly one beta-reduction using
; normal evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no normal-order redex.
; Skip over any redexes that cannot be reduced using capture avoiding subst.

;;;;;;;;;;;;;; THIS FUNCTION WORKS ;;;;;;;;;;;;;;

(define (normal-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (normal-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
                    (let ([body+ (capt-avoid-subst body y ea)])
                      (if (eq? body+ 'failed)
                          (set) ; reducing redex failed
                          (set body+)))]
         [`(,ef ,ea)
          (define ea-st (normal-order-reduce ea))
          (if (set-empty? ea-st)
              ; Call expression contains no redex
              (let ([ef-st (normal-order-reduce ef)])
                (if (set-empty? ef-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,(set-first ef-st) ,ea))))
              ; Call expression contained a redex
              (set `(,ef ,(set-first ea-st))))]))

; Reduce the given expression by exactly one beta-reduction using
; call-by-value evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no CBV order redex.

;;;;;;;;;;;;;; THIS FUNCTION WORKS ;;;;;;;;;;;;;;

(define (CBV-order-reduce e)
 (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (CBV-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
          (define body-st (set))
          (if (set-empty? body-st)
              ; No redex under this lambda:
              (let ([ea-st (CBV-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; This is the innermost+leftmost redex
                    (let ([body+ (capt-avoid-subst body y ea)])
                      (if (eq? body+ 'failed)
                          (set) ; reducing redex failed
                          (set body+)))
                    ; A redex under the argument expression was found
                    (set `((lambda (,y) ,body) ,(set-first ea-st)))))
              ; A redex under the lambda was reduced already
              (set `((lambda (,y) ,(set-first body-st)) ,ea)))]
         [`(,ef ,ea)
          (define ef-st (CBV-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (CBV-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))

; Reduce the given expression by exactly one beta-reduction using
; call-by-name evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no applicative order redex.

; (CBN-order-reduce '((lambda (q) ((lambda (id) ((lambda (x) x) id)) ((lambda (y) y) (lambda (z) z)))) ((lambda (u) (u u)) (lambda (r) r))))

;;;;;;;;;;;;;; THIS FUNCTION WORKS ;;;;;;;;;;;;;;

(define (CBN-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (normal-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
                    (let ([body+ (capt-avoid-subst body y ea)])
                      (if (eq? body+ 'failed)
                          (set) ; reducing redex failed
                          (set body+)))]
         [`(,ef ,ea)
          (define ea-st (normal-order-reduce ea))
          (if (set-empty? ea-st)
              ; Call expression contains no redex
              (let ([ef-st (normal-order-reduce ef)])
                (if (set-empty? ef-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,(set-first ef-st) ,ea))))
              ; Call expression contained a redex
              (set `(,ef ,(set-first ea-st))))]))
(trace CBN-order-reduce)


; Takes one of the four step/reduce functions and an expression in the lambda calculus (satisfying exp?)
; Yields a list representing the full evaluation trace from e to a value
; Note, this function will non-terminate on programs like Omega that cannot be reduced to a value.
(define (collect-evaluation-trace step-f e)
  (let loop ([latest (set e)]
             [trace '()])
    (if (set-empty? latest)
        (reverse trace)
        (loop (step-f (set-first latest))
              (cons (set-first latest) trace)))))

