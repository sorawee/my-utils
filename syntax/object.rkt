#lang racket/base

(require racket/function
         racket/list
         syntax/parse
         loop)

(provide (all-defined-out)
         (all-from-out 'with-contracts))

(module with-contracts racket/base
  (require racket/pretty
           racket/contract
           syntax/parse
           fancy-app)
  (provide (contract-out [occurs-in? (-> syntax? identifier? boolean?)]
                         [path-string->string (-> (or/c string? path?) string?)]
                         [stx->string (-> syntax? string?)]
                         [stx-flatten-id (-> syntax? (listof identifier?))]
                         [free-id-in? (-> identifier? syntax? any/c)]))

  (define (occurs-in? stx sym)
    (syntax-parse stx
      [_:id (free-identifier=? stx sym)]
      [(a . b) (or (occurs-in? #'a sym) (occurs-in? #'b sym))]
      [_ #f]))

  (define (path-string->string p)
    (cond
      [(path? p) (path->string p)]
      [else p]))

  (define (stx->string stx) (pretty-format (syntax->datum stx)))

  (define stx-flatten-id
    (syntax-parser
      [id:id (list #'id)]
      [(a . b) (append (stx-flatten-id #'a) (stx-flatten-id #'b))]
      [_ '()]))

  (define (free-id-in? x xs)
    (memf ((free-identifier=? _ x) (stx-flatten-id xs))))
  )

(require 'with-contracts)

;; measure the length of an improper list
;; where the last pair counts as one
(define (length-improper xs)
  (cond
    [(cons? xs) (add1 (length-improper (cdr xs)))]
    [else 0]))

(define current-replacements (make-parameter '()))

(define (rebuild expr replacements #:assoc [asso assq] #:collect? [collect? #f])
  (loop lp ([expr expr] [same-k (thunk expr)] [diff-k identity])
    (let ([a (asso expr replacements)])
      (cond
        [a (when collect?
             (current-replacements (cons (car a) (current-replacements))))
           (diff-k (cdr a))]
        [(pair? expr)
         (lp (car expr)
             (thunk
              (lp (cdr expr) same-k
                  (λ (y) (diff-k (cons (car expr) y)))))
             (λ (x)
               (lp (cdr expr)
                   (thunk (diff-k (cons x (cdr expr))))
                   (λ (y) (diff-k (cons x y))))))]
        [(vector? expr)
         (lp (vector->list expr) same-k
             (λ (x) (diff-k (list->vector x))))]
        [(box? expr)
         (lp (unbox expr) same-k (λ (x) (diff-k (box x))))]
        [(syntax? expr)
         (if (identifier? expr)
             (same-k)
             (lp (syntax-e expr) same-k
                 (λ (x) (diff-k (datum->syntax expr x expr expr)))))]
        [else (same-k)]))))

(define (keep-lambda-properties orig new)
  (let ([p (syntax-property orig 'method-arity-error)]
        [p2 (syntax-property orig 'inferred-name)])
    (let ([new (if p
                   (syntax-property new 'method-arity-error p)
                   new)])
      (if p2
          (syntax-property new 'inferred-name p2)
          new))))

(define (make-compiler transformer #:base [base-compiler (current-compile)])
  (λ (e immediate-eval?)
    (define stx
      (cond
        [(syntax? e) e]
        [else (namespace-syntax-introduce
               (datum->syntax #f e))]))
    (base-compiler (transformer stx) immediate-eval?)))

