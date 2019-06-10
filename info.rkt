#lang info
(define collection "my-utils")
(define deps '("fancy-app"
               "loop"
               "reprovide-lang"
               "terminal-color"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/my-utils.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sorawee))
