#lang racket

(require "../../exam.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (odd-indices '(0 1 2)))))
