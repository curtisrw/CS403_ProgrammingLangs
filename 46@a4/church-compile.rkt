#lang racket

;; Assignment 4: A church-compiler for Scheme, to Lambda-calculus

(provide church-compile
         ; provided conversions:
         church->nat
         church->bool
         church->listof)


;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)    
;     | (let ([x e] ...) e)  
;     | (let* ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (e e ...)    
;     | x  
;     | (and e ...) | (or e ...)
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f 
; nat ::= 0 | 1 | 2 | ... 
; x is a symbol
; prim is a primitive operation in list prims
; The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

; This input language has semantics identical to Scheme / Racket, except:
;   + You will not be provided code that yields any kind of error in Racket
;   + You do not need to treat non-boolean values as #t at if, and, or forms
;   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
;                                           or binary (+ - * = cons)
;   + There will be no variadic functions or applications---but any fixed arity is allowed

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;
; also as interpreted by Racket


;; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '()))) 


;; Write your church-compiling code below:

; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)

;;;;;;;;;;;;;;;;;; References ;;;;;;;;;;;;;;;;;;
;;
;; Y-combinator video lecture by Dr. Gilray
;; a4 introduction video lecture by Dr. Gilray
;; Lambda calculus notes and lectures from our class
;; Lambda calculus lectures from other classes on writing built-in functions in lambda calculus
;; StackOverflow's topic of predecessor reduction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (churchify e)
  (match e
         ;; Following the matching Input Language:
    
         ; let - provided by Dr. Gilray
         [`(let ([,x ,rhs] ...) ,body)
          (churchify `((lambda ,x ,body) . ,rhs))]

         ; (let* ([x e] ...) e)
         [`(let* ([,x ,rhs] ...) ,body)
          (churchify `(([,x ((lambda ,x ,body))]) . ,rhs))]
    
         ; (letrec ([x (lambda (x ...) e)]) e)
         [`(letrec ([,e (lambda (,x ...) ,ef)]) ,earg)
          (churchify `(let ([,e (y-comb (lambda (,e) (lambda ,x ,ef)))]) ,earg))]

         ; (if e e e)
         [`(if ,ef ,earg ,eargs)
          (churchify `(,ef (lambda () ,earg) (lambda () ,eargs)))]

         ; x as a symbol
         [(? symbol? x) x]

         ; literals - provided by Dr. Gilray		  
         [(? integer? n)
            (define (wrap n)
              (if (zero? n)
                  'x
                  `(f ,(wrap (- n 1)))))
            (churchify `(lambda (f x) ,(wrap n)))]
    
         ; null or (quote ())
         [''()
          (churchify `(lambda (when-cons when-null) (when-null)))]
    
         ; true
         [#t  
          (churchify `(lambda (t f) (t)))]
    
         ; false
         [#f  
          (churchify `(lambda (t f) (f)))]

         ; (and e ...)  
         [`(and ,ef ,earg)
          (churchify `(if ,ef (if ,earg #t #f) #f))]

         ; (or e ...)
         [`(or ,ef ,earg)
          (churchify `(if ,ef #t (if ,earg #t #f)))]
    

         ; currying lambdas - provided by Dr. Gilray
         [`(lambda () ,body)
          `(lambda (_) ,(churchify body))]
         [`(lambda (,x) ,body)
          `(lambda (,x) ,(churchify body))]
         [`(lambda (,x ,ys ...) ,body)
          `(lambda (,x) ,(churchify `(lambda ,ys ,body)))]


         ; curry applications - provided by Dr. Gilray
         [`(,ef)
          `(,(churchify ef) (lambda (x) x))]
         [`(,ef ,earg)
          `(,(churchify ef) ,(churchify earg))]
         [`(,ef ,earg0 ,eargs ...)	
           (churchify `((,ef ,earg0) . ,eargs))]

    
         ; catch anything that doesn't match the above cases
         [_ e]))

; Takes a whole program in the input language, and converts it into an equivalent program in lambda-calc
(define (church-compile program)
  ; Define primitive operations and needed helpers using a top-level let form?
  ; +
  (define church+ `(lambda (m n) (lambda (f x) ((n f) ((m f) x)))))

  ; *
  (define church* `(lambda (m n) (lambda (f x) ((m (n f)) x))))

  ; zero?
  (define church-zero `(lambda (m) ((m (lambda (x) #f)) #t)))

  ; add1
  (define church-add1 `(lambda (m) (lambda (f x) (f ((m f) x)))))

  ; null?
  (define church-null `(lambda (n) (n (lambda (a b) #f) (lambda () #t))))

  ; cons
  (define church-cons `(lambda (x y) (lambda (when-cons when-null) (when-cons x y))))

  ; car
  (define church-car `(lambda (c) (c (lambda (x y) x) (lambda () (lambda (u) u)))))

  ; cdr
  (define church-cdr `(lambda (c) (c (lambda (x y) y) (lambda () (lambda (u) u)))))
  
  ; not
  (define church-not `(lambda (b) (if b #f #t)))
  
  ; Notes from y-combinator video lecture
  
  ; (Y f) == (f (Y f)) --> Y = (lambda (f) (f (Y f))) --> Y = (U (lambda (y) (lambda (f) (f ((U y) f)))))
  ;
  ;                                        |
  ;                                        |
  ;                                        V
  ;
  ; Y = (U (lambda (y) (lambda (f) (f (lambda (x) (((U y) f) x))))))
  
  (define y-comb `((lambda (u) (u u)) (lambda (y) (lambda (f) (f (lambda (x) (((y y) f) x)))))))

 
  ;;;;;;;;;;;;;;;;;; Extra Credit ;;;;;;;;;;;;;;;;;;

  ; sub1
  (define church-sub1 `(lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x))(lambda (u) u))))))

  ; -
  (define church- `(lambda (m n) ((n ,church-sub1) m)))

  ; = 
  (define church= `(lambda (m n) (and (,church-zero (,church- m n)) (,church-zero (,church- n m)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  (churchify
   `(let ([add1 ,church-add1]
          [+ ,church+]
          [* ,church*]
          [zero? ,church-zero]
          [null? ,church-null]  	
          [cons ,church-cons]
          [car ,church-car] 
          [cdr ,church-cdr]  		  	  
          [not ,church-not]
          [y-comb ,y-comb]
          [sub1 ,church-sub1]
          [- ,church-]
          [= ,church=])
      ,program)))
