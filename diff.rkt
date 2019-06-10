#lang racket/base

(provide (all-defined-out))
(require racket/set
         racket/file
         racket/function
         racket/match
         racket/system
         "./define.rkt")

(define-multiple
  #:prefix current-diff-
  #:trans (make-parameter)
  [context 3]
  [column 80])

(define (print-diff a b)
  (define path-a (make-temporary-file))
  (define path-b (make-temporary-file))
  (display-to-file a path-a
                   #:mode 'text
                   #:exists 'replace)
  (display-to-file b path-b
                   #:mode 'text
                   #:exists 'replace)

  (match-define
    (list in out _ err proc)
    (process* (find-executable-path "sdiff")
              "-d" "-b" "-t" "-w" (number->string (* 2 (current-diff-column)))
              (path->string path-a)
              (path->string path-b)))

  (close-output-port out) ; we don't use it

  (define show-set (mutable-set))

  (define lines
    (for/list ([line (in-lines in)] [i (in-naturals)])
      (when (not (char=? (string-ref line (sub1 (current-diff-column))) #\space))
        (for ([j (in-range (- i (current-diff-context))
                           (+ i (current-diff-context) 1))])
          (set-add! show-set j)))
      line))

  (close-input-port in)
  (close-input-port err)

  (delete-file path-a)
  (delete-file path-b)

  (for ([line lines] [i (in-naturals)] #:when (set-member? show-set i))
    (when (not (set-member? show-set (sub1 i)))
      (displayln (make-string (* 2 (current-diff-column)) #\-)))
    (displayln line))
  (when (not (set-empty? show-set))
    (displayln (make-string (* 2 (current-diff-column)) #\-))))
