#lang racket

(require racket/function)
(require parsack)
;(require (for-syntax parsack racket/base))

; ------------------------------------------------------------------------------
; Parse Markdown with into a readable sexpr format
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
     (let ((reps (build-list (syntax->datum #'n) (λ (_) #'parser))))
         #`(parser-seq #,@reps))]))

(parse-result  (>> (repeat 3 (char #\-)) $newline) "---\n")

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

  (define parse-string ...)

  (define parse-string-lit ...)
  (define parse-num-lit ...)
  (define parse-standard-date ...)
  (define parse-yaml-list ...
    (define parse-yaml-list1 ...))
  (define parse-yaml-kvs ...)

)

(define markdown-parser ...)

(define blog-post-parser ...)
