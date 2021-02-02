#lang racket

(require "../../e7.rkt")

(with-output-to-file "output"
  (lambda ()
    (print (cons (fibonacci-generator)
                 (cons (fibonacci-generator)
                       (cons (fibonacci-generator)
                             (cons (fibonacci-generator)
                                   (cons (fibonacci-generator)
                                         '())))))))
  #:exists 'replace)
