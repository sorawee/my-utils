#lang racket/base

(provide (all-defined-out))

;; just like binding in maybe monad
(define (bind x f)
  (cond
    [x => f]
    [else #f]))
