#lang racket/base

(provide (all-defined-out))
(require racket/function
         racket/list
         "./debug.rkt"
         "./double.rkt")

(define (pick-in-list xs
                      #:treshold [treshold 0.9]
                      #:measure [measure (const #f)])
  (define candidates (if (procedure? measure)
                         (for/list ([x xs]) (double (measure x) x))
                         (for/list ([x xs] [y measure]) (double y x))))
  (define-values (strict-candidates non-strict-candidates) (partition fst candidates))
  (define candidate
    (cond
      [(and (not (empty? strict-candidates)) (< (random) treshold))
       ;; we are picking strict candidates
       (define pick (random (apply + (map fst strict-candidates))))
       (define picked-candidate
         (findf (λ (candidate)
                  (begin0 (< pick (fst candidate))
                    (set! pick (- pick (fst candidate))))) candidates))
       picked-candidate]
      [else
       (list-ref non-strict-candidates (random (length non-strict-candidates)))]))
  (snd candidate))
