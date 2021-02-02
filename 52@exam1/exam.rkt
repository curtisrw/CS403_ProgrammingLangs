#lang racket

;; Coding Exam 1

(require racket/trace)
(provide only-integers
         power-set
         list->polynomial
         interp-ce-ext)


;; Problem 0: Filter a list of numbers to contain only integers (in the same order as given)
;;
;; E.g., (only-integers '(0 5 7.2 6.0 3.14159 1/3)) => '(0 5 6.0)

(define (only-integers lst)
  (if (empty? lst)
      '()
      (if (integer? (car lst))
          (cons (car lst) (only-integers (cdr lst)))
          (only-integers (cdr lst)))))

; (trace only-integers)


;; Problem 1: Computing a power-set 
;;
;; Your task is to write a function taking a set and returning its power set
;;
;; E.g., (power-set (set 1 2)) => (set (set) (set 1) (set 2) (set 1 2))

(define (power-set st)
  (for/fold ([out (set (set))]) ([item st])
    (set-union out 
               (list->set (set-map out
                                   (lambda (x) (set-add x item)))))))


;; Problem 2: Turn a list of coefficients into a polynomial function
;;
;; Your task is to write a function list->polynomial that takes a list of
;; number?-satisfying coefficients (ordered from degree zero, one, two, on up) 
;; and returns a single-arity function encoding the polynomial.
;;
;; E.g., (list->polynomial '(2 3 4)) should return a value equivalent to
;;    => (lambda (x) (+ (* 4 x x) (* 3 x) 2))
      

(define (list->polynomial lst)
  (lambda (x)
    (if (empty? lst)
        0
        (if (= (length lst) 1)
            (car lst)
            (+ (car lst) (* x ((list->polynomial (rest lst)) x)))))))


;; Problem 3: Debugging a CE interpreter [***]
;;
;; Your task is to write a CE interpreter for the language
;; lambda-ext?, which should implement the call-by-name (CBN) lambda
;; calculus with several extensions. To do this, you will fix the
;; provided starter code in interp-ce-ext. This function contains
;; three bugs. You will fix these bugs so that interp-ce-ext properly
;; interprets the language lambda-ext? using the CBN evaluation
;; strategy.

;; prims include +, -, *, /, >, and <
(define (prim? x) (member x '(+ - * / < >)))

(define (lambda-ext? expr)
  (match expr
    ; Lambdas
    [`(lambda (,(? symbol? x)) ,(? lambda-ext? e-body)) #t]
    ; Variable reference
    [(? symbol? x) #t]
    ; Boolean and integer constants
    [(? boolean? b) #t]
    [(? integer? i) #t]
    ; If
    [`(if ,(? lambda-ext? e-guard) ,(? lambda-ext? e-true) ,(? lambda-ext? e-false)) #t]
    ; Binary primitives
    [`(,(? prim? operator) ,(? lambda-ext? e0) ,(? lambda-ext? e1)) #t]
    ; Applications
    [`(,(? lambda-ext? e0) ,(? lambda-ext? e1)) #t]))

(define (interp-ce-ext exp [env (hash)])
  ; generate a racket function corresponding to the prim
  (define (prim->impl prim)
    (hash-ref (hash '+ + '- - '* * '/ / '< < '> >) prim))     
  (match exp
    ; lambda
    [`(lambda (,x) ,body)
     ; return a closure that pairs the code and current (definition) environment
     `(closure (lambda (,x) ,body) ,env)]
    ; variables
    [(? symbol? x)
     ; look up variable in the current environment
     (hash-ref env x)]
    ; boolean and integer constants
    [(? boolean? b) b]
    [(? integer? i) i]
    ; if
    [`(if ,(? lambda-ext? e-guard) ,(? lambda-ext? e-true) ,(? lambda-ext? e-false))
     (define vguard (interp-ce-ext e-guard env))
     (define vtrue (interp-ce-ext e-true env))
     (define vfalse (interp-ce-ext e-false env))
     (if `(interp-ce-ext ,vguard env)
         `(interp-ce-ext ,vtrue env)
         `(interp-ce-ext ,vfalse env))]
    ; binary primitives
    [`(,(? prim? prim) ,(? lambda-ext? e0) ,(? lambda-ext? e1))
     ((prim->impl prim) (interp-ce-ext e0 env) (interp-ce-ext e1 env))]
    ; applications
    [`(,efun ,earg)
     (define vfun (interp-ce-ext efun env))
     (define varg (interp-ce-ext earg env))
     (match-define `(closure (lambda (,x) ,body) ,env+) efun)
     (interp-ce-ext body (hash-set env x earg))]))

; (trace interp-ce-ext)
