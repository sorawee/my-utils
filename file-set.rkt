#lang racket/base

(provide (all-defined-out))
(require racket/file
         racket/function)

(define (make-file-set name)
  (define path (make-temporary-file (string-append name "~a")))
  (define (member? datum)
    (with-input-from-file path
      (thunk (for/or ([x (in-port)]) (equal? x datum)))))
  (define (record! datum)
    (with-output-to-file path #:mode 'text #:exists 'append
      (thunk (writeln datum))))
  (values member? record!))
