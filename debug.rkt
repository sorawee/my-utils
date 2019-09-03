#lang racket/base

(provide (all-defined-out))
(require racket/format
         racket/pretty
         syntax/parse/define
         terminal-color
         "./define.rkt"
         "./syntax/pretty-syntax-format.rkt"
         (for-syntax racket/base))

(define-multiple
  #:prefix current-debug-
  #:trans (make-parameter)
  [fg 'red]
  [bg 'default])

(define (debug/core x msg-backup
                    #:msg [msg #f]
                    #:fg [fg (current-debug-fg)]
                    #:bg [bg (current-debug-bg)])
  (displayln-color
   (format "~a: ~a"
           (or msg msg-backup)
           (cond
             [(syntax? x) (pretty-syntax-format x)]
             [else (pretty-format x)]))
   #:fg fg #:bg bg)
  x)

(define-simple-macro (debug: x opt ...)
  (debug/core x 'x opt ...))

(define (assert x #:msg [msg #f] . ctx)
  (when (not x)
    (displayln-color (format "ASSERTION ERROR~a" (or (~a ": " msg) ""))
                     #:fg 'red #:bg 'white)
    (for ([e ctx])
      (displayln-color (format "% ~a" (pretty-format e)) #:fg 'red #:bg 'white))
    (error 'assert)))
