#lang racket

(require racket/function)
(require parsack)
(require (for-syntax racket/function))

; ------------------------------------------------------------------------------
; Parse Markdown into a readable sexpr format
;
; MILESTONE 1: be able to migrate my old blog posts
;   - write grammar for this sexpr.
;   - write a lightweight sexpr format that can be easily transformed
;     into the full (markuped format)
;   - serialize the markuped format into html
;
; MILESTONE 2: extend markdown with inlinable sexprs

(define-syntax (parser-rep stx)
  (syntax-case stx ()
    [(_ n parser) ; =>
     (let ((reps (build-list (syntax->datum #'n) (const #'parser))))
       #`(parser-seq #,@reps))]))

(define ret-number (compose string->number list->string))
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
(define (nothing? m) (not (maybe-filled? m)))

(define maybe-mempty nothing)

; maybe a -> maybe b -> (a -> b -> c) -> maybe c
(define (maybe-mplus m1 m2 #:combiner [f cons])
  (cond
    [(nothing? m1) m2]
    [(nothing? m2) m1]
    [else (f (from-just m1) (from-just m2))]))

(define (maybe-mconcat maybes)
  (foldr maybe-mplus maybe-mempty maybes))

(struct yaml-kv (key val))

(define yaml-block-parser
  (let ()
    (define BEGIN-END (>> (parser-rep 3 (char #\-)) $newline))
    
    (define parse-ident
      (parser-compose
       (c  <- (satisfy char-lower-case?))
       (cs <- (many (satisfy (λ (c)
                               (or (char-alphabetic? c)
                                   (char-numeric? c)
                                   (char=? #\_ c)
                                   (char=? #\- c))))))
       (return (list->string (cons c cs)))))
    
    (define parse-string-lit
      ; string can be either single or double quotes
      (parser-compose
       (open <- (<or> (char #\") (char #\')))
       (str  <- (many $anyChar 
                      #:till (if (char=? open #\")
                                 (char #\")
                                 (char #\')))) 
       (return (list->string str))))
    
    (define parse-num-lit       
      (let ([parse-number (many1 $digit)])        
        (>>= parse-number
             (λ (int)
               (<or>
                (>>= (parser-cons (char #\.) parse-number)                    
                     (λ (frac) (return (ret-number (append int frac)))))
                (return (ret-number int)))))))
    
    
    #;(module+ test
        (parse-result parse-num-lit "1234")
        (parse-result parse-num-lit "1234.5678")) 
    
    (define parse-date
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

    
    (define parse-yaml-list
      (let ()
        (define parse-yaml-list1
          (parser-one
           (char #\-) $space
           ; keeps squiggle arrow only
           (~> (<or> parse-ident
                     parse-num-lit
                     parse-date
                     parse-string-lit))
           $eol))
        
        (many1 parse-yaml-list1)))
    
    ; ... -> maybe [kv]
    (define parse-yaml-kvs
      (let ()
        (define parse-yaml-kv1
          (parser-compose
           (k <- parse-ident)
           (char #\:)
           $spaces
           (v <- (<or> parse-ident
                       parse-num-lit
                       parse-date
                       parse-string-lit
                       (>> $eol parse-yaml-list)
                       (return nothing)
                       ))
           (return (if (nothing? v) nothing
                       (just (yaml-kv k v))))))
        
        (maybe-mconcat (many1 parse-yaml-kv1))))
    
    '()
    ))

#;(define markdown-parser ...)

#;(define blog-post-parser ...)
