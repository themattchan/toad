#lang racket

(require "parser-extras.rkt"
         #;"maybe.rkt")

(provide p/yaml-block)

(module+ test
  (require rackunit)

  (define-syntax run
    (syntax-rules  (=>)
      [(_ parser (string => expect) ...) ; =>
       (let ()
         (check-equal? (parse-result parser string) expect)
         ...)]))
  )


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

; Helpers and data structures

(define (mk-date d m y)
  (date 0 0 0
        d m y
        0 0 #f 0))

;(struct yaml-kv (key val) #:transparent)

; Parsers

(define BEGIN-END (>> (parser-rep 3 (char #\-)) $eol))

(define p/ident
  ; idents are atoms
  (parser-compose
   (c  <- (satisfy char-lower-case?))
   (cs <- (many (satisfy (λ (c)
                           (or (char-alphabetic? c)
                               (char-numeric? c)
                               (char=? #\_ c)
                               (char=? #\- c))))))
   (return (string->symbol (list->string (cons c cs))))))

(module+ test
  (run p/ident
       ("foo"          => 'foo)
       ("foo-bar"      => 'foo-bar)
       ("foo_bar"      => 'foo_bar)
       ("foo-bar-1234" => 'foo-bar-1234)))

  
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

(module+ test
  (run p/num-lit
       ("1234" => 1234)
       ("1234.5678" => 1234.5678)))

(define p/date
  ; put date in standard Racket date struct
  (parser-compose
   (y <- (parser-rep 4 $digit))
   (char #\-)
   (m <- (parser-rep 2 $digit))
   (char #\-)
   (d <- (parser-rep 2 $digit))
   (return (mk-date
            (ret-number d)
            (ret-number m)
            (ret-number y)))))

(module+ test
  (run p/date
       ("2016-04-01" => (mk-date 01 04 2016))))

(define p/yaml-list
  (let ()
    (define parse-yaml-list1
      (parser-one
       (char #\-) $blanks
       ; keeps squiggle arrow only
       (~> (<or> p/ident
                 p/date
                 p/num-lit                 
                 p/string-lit))
        (try (<or> $eol $eof))))
    
    (many1 parse-yaml-list1)))


(module+ test  
  (run p/yaml-list
       ("- foo\n- bar\n- \"hello world\"\n"
        => '(foo bar "hello world"))))

#;(define parse-yaml-kv1
  (parser-compose
   (k <- p/ident)
   (char #\:)
   $blanks
   (v <- (<or>
          (<any> (>> $eol p/yaml-list)
                p/ident
                p/date
                p/num-lit                  
                p/string-lit)
          (>> $eol FAIL)
          (>> $eof FAIL)))

   
;   (try (<or> $eol $eof))

   
   (return (success? v (cons k v)))))

(define parse-yaml-kv1
  (>>= p/ident
       (λ (id)
         (>> (char #\:)
             (<or>
              (try (>>= (try (>> $spaces p/yaml-list))
                        (λ (ls) (return (cons id ls)))))
              
              (try (>>= (>> $blanks
                            (<or>
                             (try p/ident)
                             (try p/date)
                             (try p/num-lit)
                             (try p/string-lit)))
                        (λ (atom) (return (cons id atom)))))
              
              (try (>>= (try (<or> (>> $blanks $eol)
                                   (>> $blanks $eof)))
                        (const (return null)))))))))

;)))))
       

(module+ test
  (run parse-yaml-kv1
       ("foo: \"bar\""
        => (cons 'foo "bar"))
       #;("things:\n- ident1\n- \"this is a long string\"\n- \"lorem ipsum dolor sit amet\"\n"
        => (cons 'things
                 (list 'ident1
                       "this is a long string"
                       "lorem ipsum dolor sit amet")))
       #;("foo:   \n" => '(()))))

(define p/yaml-kvs
  (>>= (many1 (parser-seq parse-yaml-kv1 (~ $spaces)))
       (compose return list/mconcat)))
         
(module+ test
  (run p/yaml-kvs
       ("foo: \"bar\"\ndate: 2016-04-01\n"
       => (list (cons 'foo "bar")
                (cons 'date (mk-date 01 04 2016))))
       ("foo:   \nbar: frak" => `(,(cons 'bar 'frak)))
       ("bar: frak\nfoo:   \nbar: frak" => (list (cons 'bar 'frak) (cons 'bar 'frak)))))

(define p/yaml-block
  (between BEGIN-END BEGIN-END p/yaml-kvs))

#;(parse-result
 p/yaml-block
"---
layout: post
title: \"Thinking with types, an example\"
subtitle:
date: 2014-12-12
updated: 2016-03-12
tags:
- functional-programming
---
")
 
