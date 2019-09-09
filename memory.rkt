#lang racket/base

(provide (all-defined-out))
(require racket/format
         syntax/parse/define
         terminal-color
         (for-syntax racket/base))

(define (measure-memory)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (current-memory-use))

(define (get-gb v)
  (format "~a GB" (~r (/ v 1024 1024 1024) #:precision 2)))

(define-simple-macro (with-measure-memory . body)
  (let ([now (measure-memory)]
        [ret (let () . body)])
    (displayln-color (format "~a: ~a"
                             (pretty-format 'body)
                             (get-gb (- (measure-memory) now)))
                     #:fg 'yellow)
    ret))
