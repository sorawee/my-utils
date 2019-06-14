#lang info
(define collection "my-utils")
(define deps '("fancy-app"
               "kw-make-struct"
               "threading"
               "loop"
               "reprovide-lang"
               "terminal-color"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/my-utils.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sorawee))
