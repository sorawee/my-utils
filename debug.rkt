#lang racket/base

(provide (all-defined-out))
(require racket/format
         racket/pretty
         syntax/to-string
         syntax/parse/define
         terminal-color
         "./define.rkt"
         (for-syntax racket/base))

(define-multiple
  #:prefix current-debug-
  #:trans (make-parameter)
  [fg 'red]
  [bg 'default])

(define (debug x
               #:msg [msg #f]
               #:fg [fg (current-debug-fg)]
               #:bg [bg (current-debug-bg)])
  (displayln-color
   (format "~a: ~a"
           (or msg "Debug")
           (cond
             [(syntax? x) (~a "<pretty-syntax: " (pretty-format (syntax->datum x)) ">")]
             [else x]))
   #:fg fg #:bg bg)
  x)

(define-simple-macro (debug* x opt ...)
  (debug x #:msg 'x opt ...))

(define (assert x #:msg [msg #f] . ctx)
  (when (not x)
    (displayln-color (format "ASSERTION ERROR~a" (or (~a ": " msg) ""))
                     #:fg 'red #:bg 'white)
    (for ([e ctx])
      (displayln-color (format "% ~a" (pretty-format e)) #:fg 'red #:bg 'white))
    (error 'assert)))
