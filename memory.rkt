#lang racket/base

(provide (all-defined-out))
(require racket/format)

(define (get-gb x) (~a (~r (/ x 1024 1024 1024) #:precision 1) " GB"))

(define (measure-memory)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (current-memory-use))
