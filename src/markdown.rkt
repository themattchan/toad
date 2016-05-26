#lang racket

(require ;net/url
         scribble/base
         "parser-extras.rkt"
         "yaml.rkt")

; ------------------------------------------------------------------------------
; Parse Markdown into Scribble.
;
; MILESTONE 1: be able to migrate my old blog posts
;   - write grammar for this sexpr.
;   - write a lightweight sexpr format that can be easily transformed
;     into the full (markuped format)
;   - serialize the markuped format into html
;
; MILESTONE 2: extend markdown with inlinable sexprs

; GRAMMAR
;
;- inline html
;- escaping & characters
;- paragraphs   --> para
;- headings     --> 
;- bold/italic... etc
;- blockquotes
;- backticks
;- codeblocks
;- footnotes
;- lists
;- links

(define p/link ...)
(define p/url-literal ...)
(define p/itemize ...)
(define p/enumerate ...)
(define p/pound-heading ...)
(define p/underline-heading ...)
(define p/inline-code-escape ...)
(define p/backtick-code-block ...)
(define p/paragraph ...)
(define p/bold ...)
(define p/italic ...)
(define p/underline ...)


#;(define markdown-parser ...)

#;(define blog-post-parser ...)
