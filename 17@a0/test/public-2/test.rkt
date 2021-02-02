#lang racket

(require "../../a0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (implies-value #t #t))))
