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

(define-syntax (repeat stx)
  (syntax-case stx ()
    [(_ n parser) ; =>
     (let ((reps (build-list (syntax->datum #'n) (const #'parser))))
       #`(parser-seq #,@reps))]))

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


(define yaml-block-parser
  (let ()
    (define BEGIN-END (>> (repeat 3 (char #\-)) $newline))
    
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
      (let ([parse-number (many1 $digit)]
            [to-number (λ (nls) (string->number (list->string nls)))])        
        (>>= parse-number
             (λ (int)
               (<or>
                 (>>= (parser-cons (char #\.) parse-number)                    
                      (λ (frac) (return (to-number (append int frac)))))
                (return (to-number int)))))))
             
            
#;(module+ test
        (parse-result parse-num-lit "1234")
        (parse-result parse-num-lit "1234.5678")) 
    
   ;(define parse-standard-date ...)
   ; (define parse-yaml-list ...
   ;   (define parse-yaml-list1 ...))
   ; (define parse-yaml-kvs ...)
    '()
))

#;(define markdown-parser ...)

#;(define blog-post-parser ...)
