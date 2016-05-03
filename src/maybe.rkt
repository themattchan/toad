#lang typed/racket

; Maybe Monoid
; TODO: extract generic interface, modularize

(require racket/match
         "typeclasses.rkt")

(define-type (Maybe a) (U None (Just a)))
(struct None ())
(struct (a) Just ([v : a]) #:transparent)

(: mplus (All [a b c]
              (->* ((Maybe a) (Maybe b))
                   (#:combiner (-> a b c))
                   ; so much for strong typing
                   (Maybe c))))
(define (mplus m1 m2 #:combiner [f cons])
  (match `(,m1 ,m2)
    [`((None) _) m2]
    [`(_ (None)) m1]
    [(list (Just v1) (Just v2))
     (Just (f v1 v2))])) 

 
#|

(define (just x) (maybe #t x))
(define (from-just x)
  (when (maybe? x)
    (maybe-thingy x)))
(define nothing (maybe #f '()))
(define (nothing? m)
  (if (maybe? m)
      (not (maybe-filled? m))
      #f))
|#

;(define maybe-mempty nothing)

#|
; maybe a -> maybe b -> (a -> b -> c) -> maybe c
(define (maybe-mplus m1 m2 #:combiner [f cons])
  (cond
    [(nothing? m1) m2]
    [(nothing? m2) m1]
    [else (f (from-just m1) (from-just m2))]))

(define (maybe-mconcat maybes)
  (foldr maybe-mplus maybe-mempty maybes))
(maybe-mconcat (list (just 1) (just 2) nothing))
|#