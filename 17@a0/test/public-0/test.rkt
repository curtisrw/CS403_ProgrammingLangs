#lang racket

(require "../../a0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (median-of-three 1 7 3))))
