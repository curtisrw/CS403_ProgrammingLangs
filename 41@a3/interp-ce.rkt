#lang racket

;; Assignment 3: A CE (Control and Environment) interpreter for Scheme

(provide interp-ce)

; Your task is to write a CE interpreter for a substantial subset of Scheme/Racket. 
; A CE interpreter is meta-circular to a large degree (e.g., a conditional in the target
; language (scheme-ir?) can be implemented using a conditional in the host language (Racket),
; recursive evaluation of a sub-expression can be implemented as a recursive call to the
; interpreter, however, it's characterized by creating an explicit closure value for lambdas
; that saves its static environment (the environment when it's defined). For example, a CE
; interpreter for the lambda calculus may be defined:
(define (interp-ce-lambda exp [env (hash)])
  (match exp
         [`(lambda (,x) ,body)
          ; Return a closure that pairs the code and current (definition) environment
          `(closure (lambda (,x) ,body) ,env)]
         [`(,vfun ,earg)
          ; Evaluate both sub-expressions
          (define vfun (interp-ce-lambda vfun env))  
          (define varg (interp-ce-lambda earg env))
          ; the applied function must be a closure
          (match-define `(closure (lambda (,x) ,body) ,env+) vfun)
          ; we extend the *closure's environment* and interp the body
          (interp-ce-lambda body (hash-set env+ x varg))]
         [(? symbol? x)
          ; Look up a variable in the current environment
          (hash-ref env x)]))

; Following is a predicate for the target language you must support. You must support any
; syntax allowed by scheme-ir that runs without error in Racket, returning a correct value..
(define (scheme-ir? exp)
  ; You should support a few built-in functions bound to the following variables at the top-level
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (match exp
         [`(lambda ,(? (listof symbol?)) ,(? scheme-ir?)) #t] ; fixed arguments lambda
         [`(lambda ,(? symbol?) ,(? scheme-ir?)) #t] ; variable argument lambda
         [`(if ,(? scheme-ir?) ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [`(let ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(let* ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(and ,(? scheme-ir?) ...) #t]
         [`(or ,(? scheme-ir?) ...) #t]
         [`(apply ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [(? (listof scheme-ir?)) #t]
         [(? (member prim?)) #t]
         [(? symbol?) #t]
         [(? number?) #t]
         [(? boolean?) #t]
         [''() #t]
         [_ #f]))

; Interp-ce must correctly interpret any valid scheme-ir program and yield the same value
; as DrRacket, except for closures which must be represented as `(closure ,lambda ,environment).
; (+ 1 2) can return 3 and (cons 1 (cons 2 '())) can yield '(1 2). For programs that result in a 
; runtime error, you should return `(error ,message)---giving some reasonable string error message.
; Handling errors and some trickier cases will give bonus points.

; I would like to reference Dr. Gilray and his class example of a miniature version of a CE interpreter
(define (interp-ce exp)
  ; (if (member exp (scheme-ir? exp)) #t #f)
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (define (interp exp env)
    ; (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
    (match exp
      [`(lambda (,x) ,body)
          `(closure (lambda (,x) ,body) ,env)]
      [`(,vfun ,earg)
          (match (interp vfun env)
                 [`(closure (lambda (,x) ,body) ,env)
                  (interp body (hash-set env x (interp earg env)))])]
      
      [(? symbol? x) (hash-ref env x)]

      [`(let ([,xs ,es] ...) ,body) (interp body (foldl (λ (x e env+) (hash-set env+ x (interp e env))) env xs es))]

      [`(let* ([,xs ,es] ...) ,body) (interp body (foldr (λ (x e env+) (hash-set env+ x (interp e env))) env xs es))]
      
      [`(lambda (,x) ,body) (lambda (v) (interp body (hash-set env x v)))]

      
      [`(,ef ,ea)
       ((interp ef env)
       (interp ea env))]

      ; if case; still not working
      [`(if ,guard ,then ,else)
       (if (interp guard env)
           (interp then env)
           (interp else env))]
      
      [(? number? n) n]
      [(? boolean? b) b]
      [(? prim? exp) #t]

      ; List case
      [(list 'list lvp ...) list lvp]

      [`((lambda ,x (,body)) ,args ...) (lambda (v) (interp body (list env x v args)))]
      
;      [`(,vfun ,earg)
;          (match (interp vfun env)
;            [`('(list ,v)) `('(,v))])]

      ; [(? scheme-ir?) (scheme-ir? exp)]

      ; addition case
      [`(+ ,ef ,ea)
       (eval `(+ ,ef ,ea) (make-base-namespace))]

      ; subtraction case
      [`(- ,ef ,ea)
       (eval `(- ,ef ,ea) (make-base-namespace))]

      ; Mult case
      [`(* ,ef ,ea)
       (eval `(* ,ef ,ea) (make-base-namespace))]

      ; Case that checks to see if args are equal
      [`(equal? ,ef ,ea)
       (eval `(= ,ef ,ea) (make-base-namespace))]
      
      ; null case: NOT WORKING PROPERLY
      [`(null? ,arg)
       (eval (null? arg) (make-base-namespace))]
;       (if (null? (interp arg env))
;           #t
;           #f)]

      ; Cons case
      [`(cons ,ef ,ea)
       (eval `(cons ,ef ,ea) (make-base-namespace))]

      ; Apply case; hidden server apply test still not passing
      [`(apply ,ef ,ea)
       (eval `(apply ,ef ,ea) (make-base-namespace))]

      ; And case
      [`(and ,lvp ...)
       (if (null? lvp)
         #t
         (interp lvp env))]

      ; Or case
      [`(or ,lvp ...)
       (if (null? lvp)
         #f
         (interp lvp env))]

      [''() #t]
         ; `(interp ,lvp env))]

      ; [(? scheme-ir?) (scheme-ir? exp)]
      
      ; Error message case; need to ask TA about this.  Not passing on autograder
      [_ (error (format "Error! not recognized by interpreter: ~a" exp))]))
  
  (interp exp (hash)))


;;;;;;;;;;;;;;; CODE THAT DIDN'T WORK ;;;;;;;;;;;;;;;;

; [(? scheme-ir?) (scheme-ir? exp)]

; (eval `(list ,args))]

; Might add helpers or other code here...
;  (define (interp exp env)
;    (match exp
;         [(? boolean? b) b]
;         [`(lambda (,x) ,body)
;          `(closure ,exp ,env)]
;         [`(,vfun ,earg)
;          (match (interp-ce fun env)
;                 [`(closure (lambda (,x) ,body) ,env)
;                  (interp-ce body (hash-set env x (interp-ce earg env)))])]
;         [(? symbol? x) (hash-ref env x)]
;         [(? number? n) n]
;         [`(,ef ,ea)
;           ((interp ef env)
;           (interp ea env))]
;         [`(and ,ef ,ea) 
;          (if (interp ef env)
;              (interp ea env)
;              #f)]
;         [`(or ,ef ,ea) 
;          (if (interp ef env)
;              #t
;              (interp ea env))
;      
;         [`(+ ,ef ,ea)
;           (eval `(+ ,ef ,ea) (make-base-namespace))]
;      
;         [else (error (format "Error! not recognized by interpreter: ~a" exp))]))
;           ;[`(,ef ,eargs ...)
;            ;]
;  (interp exp (hash)))
