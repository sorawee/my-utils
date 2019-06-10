#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define
         (for-syntax racket/base))

(define-simple-macro (change! x:id f:expr) (set! x (f x)))
(define (list-accum-maker)
  (let ([xs '()] [absence (gensym)])
    (λ ([x absence] #:reset [reset #f])
      (when reset (set! xs '()))
      (if (eq? absence x)
          xs
          (set! xs (cons x xs))))))

(module+ test
  (require rackunit)
  (define accum! (list-accum-maker))
  (check-equal? (accum!) '())
  (accum! 1)
  (check-equal? (accum!) '(1))
  (accum! 2)
  (check-equal? (accum!) '(2 1))
  (accum! #:reset #t)
  (check-equal? (accum!) '())
  (accum! 1)
  (check-equal? (accum!) '(1)))
