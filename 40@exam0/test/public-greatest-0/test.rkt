#lang racket

(require "../../exam.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (greatest-without-going-over '(0 1 2 3 4 5) 3))))
