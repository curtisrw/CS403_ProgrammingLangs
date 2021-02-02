#lang racket


(provide interp-CEK)


; Define interp-cek, a tail recursive (small-step) interpreter for the language:
;;;  e ::= (lambda (x) e)
;;;      | (e e)
;;;      | x
;;;      | (let ([x e]) e)   
;;;      | (call/cc e) 
;;;      | (if e e e)
;;;      | (and e e)
;;;      | (or e e)
;;;      | (not e)
;;;      | b
;;;  x ::= < any variable satisfying symbol? >
;;;  b ::= #t | #f

; You can use (error ...) to handle errors, but will only be tested on
; on correct inputs. The language should be evaluated as would the same subset 
; of Scheme/Racket. In order to implement call/cc properly, your interpreter
; must implement a stack (as opposed to using Racket's stack by making the
; interpreter directly recursive) yourself and then allow whole stacks to be
; used as first-class values, captured via the call/cc form. Because your 
; interpreter implements its own stack, it does not use Racket's stack,
; and so every call to interp-CEK must be in tail position!
; Use symbol 'halt for an initial, empty stack. When a value is returned
; to the 'halt continuation, that value is finally returned from interp-CEK.
; For first-class continuations, use a tagged `(kont ,k) form where k is the
; stack, just as in the CE interpreter you used a tagged `(closure ,lam ,env)
; form for representing closures.

; For example:
;;; (interp-CEK `(call/cc (lambda (k) (and (k #t) #f))) (hash) 'halt)
; should yield a value equal? to #t, and
;;; (interp-CEK `(call/cc (lambda (k0) ((call/cc (lambda (k1) (k0 k1))) #f))) (hash) 'halt)
; should yield a value equal? to `(kont (ar #f ,(hash 'k0 '(kont halt)) halt))


(define (interp-CEK cexp [env (hash)] [kont 'halt])
  (define (return kont v)
    (match kont
      ; code from Professor Gilray - Introduction to a5 video lecture
      [`(ar ,earg ,env ,kont)
       (interp-CEK earg env `(fn ,v ,kont))]
      [`(fn (closure (lambda (,x) ,body) ,env) ,kont)
       (interp-CEK body (hash-set env x v) kont)]
      [`(fn (kont ,k) ,kont)
       (return k v)]
      ['halt v]

      ; symbol
      [(? symbol? x)
       (return kont (hash-ref env x))]
      ; boolean
      [(? boolean? b)
       (return kont b)]
      ; (let ([x e]) e) 
      [`(let-kont ,arg ,earg ,env ,kont)
       (interp-CEK earg (hash-set env arg v) kont)]
      ;(if e e e)
      [`(if-kont ,arg ,earg ,env ,kont)
       (if v
           (interp-CEK arg env kont)
           (interp-CEK earg env kont))]
      ; (and e e)
      [`(and-kont ,arg ,env ,kont)
       (if v
           (interp-CEK arg env kont)
           (return kont v))]
      ; (or e e)
      [`(or-kont ,arg ,env ,kont)
       (if v
           (return kont v)
           (interp-CEK arg env kont))]
      ; (not e) - not working properly
      [`(not-kont ,arg ,kont)
       (if v
           (return kont v)
           (interp-CEK arg kont))]))

  (match cexp 
    ; code from Professor Gilray - Introduction to a5 video lecture
    ; lambda closure
    [`(lambda (,x) ,body)
     (return kont `(closure ,cexp ,env))]
    ; symbol
    [(? symbol? x)
     (return kont (hash-ref env x))]
    ; boolean
    [(? boolean? b)
     (return kont b)]
    ; (call/cc e)
    [`(call/cc (lambda (,cc) ,body))
     (interp-CEK body (hash-set env cc `(kont ,kont)) kont)]
    ; applications
    [`(,efun ,earg)
     (interp-CEK efun env `(ar ,earg ,env ,kont))]
    
    ; (let ([x e]) e)
    [`(let ([,arg ,earg]) ,eargs)
     (interp-CEK earg env `(let-kont ,arg ,eargs ,env ,kont))]
    ; (if e e e)
    [`(if ,arg ,earg ,eargs)
     (interp-CEK arg env `(if-kont ,earg ,eargs ,env ,kont))]
    ; (and e e)
    [`(and ,arg ,earg)
     (interp-CEK arg env `(and-kont ,earg ,env ,kont))]
    ; (or e e)
    [`(or ,arg ,earg)
     (interp-CEK arg env `(or-kont ,earg ,env ,kont))]
    ; (not e)
    [`(not ,arg)
     (interp-CEK arg env `(not-kont ,arg ,kont))]))

