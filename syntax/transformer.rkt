#lang racket/base

(provide lambda-form
         let-values-form
         leaf-form
         compound-form
         transform-disarm
         transform-identity)

(require racket/list
         racket/contract
         syntax/parse
         syntax/parse/lib/function-header
         syntax/stx
         loop
         "object.rkt")

(define code-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

(define-syntax-class lambda-form
  (pattern [fmls:formals body:expr ...+]))

(define-syntax-class let-values-form
  (pattern [([(ids:id ...) rhs:expr] ...) body:expr ...+]))

(define-syntax-class (leaf-form phase)
  #:literal-sets ([kernel-literals #:phase phase])
  (pattern {~or* quote quote-syntax #%top #%variable-reference
                #%require #%provide #%declare}))

(define-syntax-class (compound-form phase)
  #:literal-sets ([kernel-literals #:phase phase])
  (pattern {~or* #%expression #%plain-app begin begin0 if with-continuation-mark}))

(define (transform-disarm expr phase)
  (loop lp ([expr expr] [phase phase #:inherit])
    (let ([expr (syntax-disarm expr code-insp)])
      (define (lps exprs #:source [source expr] #:phase [phase phase])
        (rebuild source (stx-map (λ (e) (cons e (lp e #:phase phase))) exprs)))
      (define (lp-mod phase)
        (define shifted-expr (syntax-shift-phase-level expr (- phase)))
        (syntax-parse shifted-expr
          [(_ _ _ (_ . body))
           (syntax-shift-phase-level (lps #'body #:source shifted-expr #:phase 0)
                                     phase)]))
      (syntax-parse expr
        #:literal-sets ([kernel-literals #:phase phase])
        [_:id expr]
        [(define-values _ rhs) (lps #'(rhs))]
        [(define-syntaxes _ rhs) (lps #'(rhs) #:phase (add1 phase))]
        [(begin-for-syntax . exprs) (lps #'exprs #:phase (add1 phase))]
        [(module _ _ _) (lp-mod 0)]
        [(module* _ init _) (lp-mod (if (syntax-e #'init) 0 phase))]
        [({~var _ (leaf-form phase)} ~! . _) expr]
        [(#%plain-lambda . form:lambda-form)
         (keep-lambda-properties expr (lps #'(form.body ...)))]
        [(case-lambda form:lambda-form ...)
         (keep-lambda-properties expr (lps #'({~@ form.body ...} ...)))]
        [({~or* let-values letrec-values} . form:let-values-form)
         (lps #'(form.rhs ... form.body ...))]
        [(set! _ rhs) (lps #'(rhs))]
        [({~var _ (compound-form phase)} ~! e ...) (lps #'(e ...))]
        [_ (error 'transform "unrecognized expression form: ~.s"
                  (syntax->datum expr))]))))

(define (transform-identity stx phase) stx)
