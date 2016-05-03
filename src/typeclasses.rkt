#lang racket

(require racket/generic)

#|
; TODO: figure out how to define generic instance for constant values
#;(define-generics Monoid
  [monoid-zero ()]
  [monoid-plus m1 m2])

#;(define-generics Liftable
  [])

(define-generics Functor
  (fmap f Functor)
  #:defaults
  ([list? (define fmap map)]  
   ))

(define-generics Applicative
  (pure Applicative)
  (ap f-thing thing)
  #:defaults
  ([list?
    (define (pure x) (list x))
    (define (ap fs lst)
      (apply append
             (for/list ([f f-lst])
                (fmap lst))))]))

#;(define-generics Monad
  [])
|#

;; typeless typeclasses