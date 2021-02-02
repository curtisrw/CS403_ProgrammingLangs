#lang racket

(require "../../exam.rkt")

(define prog
  '((lambda (x) (((lambda (x) (lambda (y) x)) 3) 5)) 4))

(with-output-to-file "output"
                     (lambda ()
                       (print (interp-ce-ext prog)))
                     #:exists 'replace)
