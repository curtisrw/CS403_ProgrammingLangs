#lang racket

(require "../../e7.rkt")

(with-output-to-file "output"
  (lambda ()
    (print (stream-take factorial-stream 12)))
  #:exists 'replace)
