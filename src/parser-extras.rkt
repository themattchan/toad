#lang racket

(require racket/function)
(require parsack)
(require (for-syntax racket/function))

(provide (all-defined-out)
         (all-from-out parsack))

(define-syntax (parser-rep stx)
  (syntax-case stx ()
    [(_ n parser) ; =>
     (let ((reps (build-list (syntax->datum #'n) (const #'parser))))
       #`(parser-seq #,@reps))]))

(define ret-number (compose string->number list->string))

(define FAIL (const 'fail))

; monoid
(define (success? v ret)
  (if (not (eq? 'fail v))
      ret
      '()))

(define (list/mconcat xs)
  (apply append xs))