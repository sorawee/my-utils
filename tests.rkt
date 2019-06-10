#lang racket

(require syntax/parse/define)

(begin-for-syntax
  (define-syntax-class foo
    (pattern head:nat #:with stx #'() #:with ids #'())
    (pattern [head:foo b:nat]
             #:with stx #'([x b] . head.stx)
             #:with ids #'(x . head.ids))))

(define-simple-macro (bar x:foo)
  #:with (xs ...) #'x.ids
  (let x.stx (begin (println xs) ...)))

(define-syntax-parser foo*
  [(_ head:nat acc-b acc-v)
   #:with (xs ...) #'acc-v
   #'(let acc-b (begin (println xs) ...))]
  [(_ [head b:nat] acc-b acc-v) #'(foo* head ([x b] . acc-b) (x . acc-v))])

(define-simple-macro (bar* x)
  (foo* x () ()))

(define-for-syntax foo**
  (syntax-parser
    [head:nat #'(() ())]
    [[head b:nat]
     #:with (left right) (foo** #'head)
     #'(([x b] . left) (x . right))]))

(define-simple-macro (bar** x)
  #:with (left (right ...)) (foo** #'x)
  (let left (begin (println right) ...)))


(bar** [[[1 2] 3] 4])
