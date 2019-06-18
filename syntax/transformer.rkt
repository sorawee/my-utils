#lang racket/base

(provide lambda-body-form
         let-values-body-form
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


(define-syntax-class let-values-body-form
  (pattern [([(ids:id ...) rhs:expr] ...) body:expr ...+]))

(define-syntax-class lambda-body-form
  (pattern [fmls:formals body:expr ...+]))

(define-syntax-class (leaf-form phase)
  #:literal-sets ([kernel-literals #:phase phase])
  (pattern {~or* quote quote-syntax #%top #%variable-reference
                 #%require #%provide #%declare}))

(define-syntax-class (compound-form phase)
  #:literal-sets ([kernel-literals #:phase phase])
  (pattern {~or* #%expression #%plain-app begin begin0 if with-continuation-mark}))

(define-syntax-class (let-values-form phase)
  #:literal-sets ([kernel-literals #:phase phase])
  (pattern {~or* let-values letrec-values}))

(define (transform-disarm stx phase)
  (loop lp ([stx stx] [phase phase #:inherit])
    (let ([stx (syntax-disarm stx code-insp)])
      (define (lps stxs #:source [source stx] #:phase [phase phase])
        (rebuild source (stx-map (λ (e) (cons e (lp e #:phase phase))) stxs)))
      (define (lp-mod phase)
        (define shifted-stx (syntax-shift-phase-level stx (- phase)))
        (syntax-parse shifted-stx
          [(_ _ _ (_ . body))
           (syntax-shift-phase-level (lps #'body #:source shifted-stx #:phase 0)
                                     phase)]))
      (syntax-parse stx
        #:literal-sets ([kernel-literals #:phase phase])
        [_:id stx]
        [(define-values _ rhs) (lps #'(rhs))]
        [(define-syntaxes _ rhs) (lps #'(rhs) #:phase (add1 phase))]
        [(begin-for-syntax . stxs) (lps #'stxs #:phase (add1 phase))]
        [(module _ _ _) (lp-mod 0)]
        [(module* _ init _) (lp-mod (if (syntax-e #'init) 0 phase))]
        [(set! _ rhs) (lps #'(rhs))]
        [(#%plain-lambda . form:lambda-body-form)
         (keep-lambda-properties stx (lps #'(form.body ...)))]
        [(case-lambda form:lambda-body-form ...)
         (keep-lambda-properties stx (lps #'({~@ form.body ...} ...)))]
        [({~var _ (leaf-form phase)} ~! . _) stx]
        [({~var _ (let-values-form phase)} . form:let-values-body-form)
         (lps #'(form.rhs ... form.body ...))]
        [({~var _ (compound-form phase)} ~! . body) (lps #'body)]
        [_ (error 'transform "unrecognized expression form: ~.s at phase ~a"
                  (syntax->datum stx) phase)]))))

(define (transform-identity stx phase) stx)
