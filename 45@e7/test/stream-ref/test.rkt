#lang racket

(require "../../e7.rkt")

(with-output-to-file "output"
  (lambda ()
    (print (stream-ref factorial-stream 6)))
  #:exists 'replace)
