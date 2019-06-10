#lang racket/base

(provide struct+)
(require syntax/parse/define
         racket/match
         kw-make-struct
         (for-syntax racket/base
                     racket/syntax))

(begin-for-syntax
  (define-syntax-class field
    (pattern id:id #:with expr #'id)
    (pattern [id:id expr:expr])))

(define-simple-macro (struct+ name:id . xs)
  #:with name+ (format-id #'name "~a+" #'name)
  #:with ooo (quote-syntax ...)
  (begin
    (struct name . xs)
    (define-match-expander name+
      (syntax-parser
        [(_ :field ooo) #'(struct* name ([id expr] ooo))])
      (syntax-parser
        [(_ :field ooo) #'(make/fld name [id expr] ooo)]
        [(_ :field ooo #:base obj:expr) #'(struct-copy name obj [id expr] ooo)]))))

(module+ test
  (require rackunit)
  (struct+ foo (bar baz) #:transparent)
  (check-equal? (foo+ [bar 1] [baz 2]) (foo 1 2))
  (let ([bar 1]) (check-equal? (foo+ bar [baz 2]) (foo 1 2)))
  (check-equal? (foo+ [baz 3] #:base (foo 1 2)) (foo 1 3))
  (match-let ([(foo+ [bar x] baz) (foo 1 2)])
    (check-equal? x 1)
    (check-equal? baz 2)))
