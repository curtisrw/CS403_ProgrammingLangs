#lang racket

(require "../../exam.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (power-set (set 3 4))))
                     #:exists 'replace)

(with-output-to-file "answer"
                     (lambda ()
                       (print (set (set) (set 3) (set 4) (set 3 4))))
                     #:exists 'replace)
