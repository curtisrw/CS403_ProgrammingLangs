#lang racket

(require "../../exam.rkt")

(define rand-inputs (map (lambda (_) (random 200)) (range 10)))
(define list-polynomial (list->polynomial '(1 2 3)))
(define same-polynomial (lambda (n) (+ (* 3 n n) (* 2 n) 1)))

(with-output-to-file "output"
                     (lambda ()
                       (print (map list-polynomial rand-inputs)))
                     #:exists 'replace)

(with-output-to-file "answer"
                     (lambda ()
                       (print (map same-polynomial rand-inputs)))
                     #:exists 'replace)
