#lang racket

(require "../../exam.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (interp-ce-ext '((lambda (x) ((lambda (y) y) 2))
                                               ((lambda (x) (x x)) (lambda (x) (x x)))))))
                     #:exists 'replace)
