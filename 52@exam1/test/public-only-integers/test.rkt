#lang racket

(require "../../exam.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (only-integers '(0 5 7.2 6.0 3.14159 1/3))))
                     #:exists 'replace)
