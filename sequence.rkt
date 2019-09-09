#lang racket/base

(provide (all-defined-out))
(require racket/stream
         racket/generator
         racket/list)

(define (yield-from xs)
  (for ([x xs]) (yield x)))

;; Status: DONE
;; sequence-remove-index :: (sequence/c 'a) number? ->
;;                           (cons/c (or/c 'a #f) (sequence/c 'a))
(define (sequence-remove-index xs k)
  (let ([xs (in-producer (sequence->generator xs) (void))])
    (define selected (for/list ([i (in-range (add1 k))] [x xs]) x))
    (cond
      ;; if there are at least k elements
      [(= (add1 k) (length selected))
       (values (last selected)
               (in-generator
                (for ([i (in-range k)] [x (in-list selected)]) (yield x))
                (for ([x xs]) (yield x))))]
      ;; if there are less than k elements
      [else (values #f selected)])))

;; Status: DONE
;; in-spread :: (sequence/c (sequence/c 'a)) -> (sequence/c 'a)
(define (in-spread xs)
  (let ([xs (for/stream ([x xs]) (sequence->generator x))])
    (in-generator
     (for ([_ (in-naturals)])
       (define changed? #f)
       (for ([sub-xs (in-stream xs)])
         (define item (sub-xs))
         (when (not (void? item))
           (yield item)
           (set! changed? #t)))
       #:break (not changed?)
       (void)))))

;; Status: DONE
;; in-zigzag :: (sequence/c (sequence/c 'a)) -> (sequence/c 'a)
(define (in-zigzag xs)
  (let ([xs (for/stream ([x xs]) (sequence->generator x))])
    (in-generator
     (for ([limit (in-naturals 1)])
       (define changed? #f)
       (define current-idx -1)
       (for ([sub-xs (in-stream xs)] [i (in-range limit)])
         (set! current-idx i)
         (define item (sub-xs))
         (when (not (void? item))
           (yield item)
           (set! changed? #t)))
       #:break (and (not changed?) (> (sub1 limit) current-idx))
       (void)))))
