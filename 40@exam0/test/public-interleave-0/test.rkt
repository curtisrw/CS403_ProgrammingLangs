#lang racket

(require "../../exam.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (interleave '(0) '(1)))))
