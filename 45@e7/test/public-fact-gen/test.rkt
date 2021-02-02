#lang racket

(require "../../e7.rkt")

(with-output-to-file "output"
  (lambda ()
    (print (map (lambda (_) (factorial-generator)) (range 6))))
  #:exists 'replace)
