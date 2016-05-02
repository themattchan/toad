#lang racket

(require "parser-extras.rkt")
(provide p/yaml-block)

;(parse-result  (>> (repeat 3 (char #\-)) $newline) "---\n")

; YAML used in my Jekyll posts:
;
; <id>     ::= [a-z][\w|-]*
; <string> ::= \".*\"
; <num>    ::= \d+(\.\d+)?
; <date>   ::= \d{4}-\d{2}-\d{2}
;
; <list-item>
;          ::= \- [<id>|<string>|<num>|<date>]$
; <list>   ::= <list-item>*
;
; <kvs> ::= <id>: $    -- id with nothing
;        |  <id>: <id>
;        |  <id>: <string>
;        |  <id>: <date>
;        |  <id>: \newline
;             <list>
;
; <YAML> ::= ---
;            <kvs>*
;            ---

; Maybe Monoid
; TODO: extract generic interface, modularize

(struct maybe (filled? thingy))

(define (just x) (maybe #t x))
(define (from-just x) (maybe-thingy x))
(define nothing (maybe #f '()))
(define (nothing? m)
  (if (maybe? m)
      (not (maybe-filled? m))
      #f))


(define maybe-mempty nothing)

; maybe a -> maybe b -> (a -> b -> c) -> maybe c
(define (maybe-mplus m1 m2 #:combiner [f cons])
  (cond
    [(nothing? m1) m2]
    [(nothing? m2) m1]
    [else (f (from-just m1) (from-just m2))]))

(define (maybe-mconcat maybes)
  (foldr maybe-mplus maybe-mempty maybes))
(maybe-mconcat (list (just 1) (just 2) nothing))


(struct yaml-kv (key val) #:transparent)

(define BEGIN-END (>> (parser-rep 3 (char #\-)) $eol))

(define p/ident
  (parser-compose
   (c  <- (satisfy char-lower-case?))
   (cs <- (many (satisfy (λ (c)
                           (or (char-alphabetic? c)
                               (char-numeric? c)
                               (char=? #\_ c)
                               (char=? #\- c))))))
   (return (list->string (cons c cs)))))

(define p/string-lit
  ; string can be either single or double quotes
  (parser-compose
   (open <- (<or> (char #\") (char #\')))
   (str  <- (many $anyChar 
                  #:till (if (char=? open #\")
                             (char #\")
                             (char #\')))) 
   (return (list->string str))))

(define p/num-lit       
  (let ([p/number (many1 $digit)])        
    (>>= p/number
         (λ (int)
           (<or>
            (>>= (parser-cons (char #\.) p/number)                    
                 (λ (frac) (return (ret-number (append int frac)))))
            (return (ret-number int)))))))


#;(module+ test
    (parse-result parse-num-lit "1234")
    (parse-result parse-num-lit "1234.5678")) 

(define p/date
  (parser-compose
   (y <- (parser-rep 4 $digit))
   (char #\-)
   (m <- (parser-rep 2 $digit))
   (char #\-)
   (d <- (parser-rep 2 $digit))
   (return (date 0 0 0
                 (ret-number d)
                 (ret-number m)
                 (ret-number y)
                 0 0 #f 0))))  

(define p/yaml-list
  (let ()
    (define parse-yaml-list1
      (parser-one
       (char #\-) $space
       ; keeps squiggle arrow only
       (~> (<or> p/ident
                   p/num-lit
                   p/date
                   p/string-lit))
       $eol))
    
    (many1 parse-yaml-list1)))

; ... -> maybe [kv]
(define p/yaml-kvs
  (let ()
    (define parse-yaml-kv1
      (parser-compose
       (k <- p/ident)
       (char #\:)
       $spaces
       (v <- (<or> p/ident
                   p/num-lit
                   p/date
                   p/string-lit
                   (>> $eol p/yaml-list)
                   (return nothing)
                   ))
       (return (if (nothing? v) nothing
                   (just (yaml-kv k v))))))
    
    ;(>>=
     (many1 parse-yaml-kv1)))
         ;maybe-mconcat)))

(define p/yaml-block
  (parser-one
   ; BEGIN-END
   (~> p/yaml-kvs)
   ;BEGIN-END)
   ))

(map from-just(parse-result
 (parser-one
  ; BEGIN-END
  (~> p/yaml-kvs)
  ;BEGIN-END)
  )
 "layout: post
title: \"Thinking with types, an example\"
subtitle:
date: 2014-12-12
updated: 2016-03-12
tags:
- functional-programming
")
)