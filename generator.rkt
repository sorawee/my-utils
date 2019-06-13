#lang racket/base

(provide (all-defined-out))
(require racket/generator)

(define (yield-from xs)
  (for ([x xs]) (yield x)))
