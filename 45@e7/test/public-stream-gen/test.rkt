#lang racket

(require "../../e7.rkt")

(with-output-to-file "output"
  (lambda ()
    (define fact-gen (stream->generator factorial-stream))
    (print (map (lambda (_) (fact-gen)) (range 12))))
  #:exists 'replace)
