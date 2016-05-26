#lang racket

(require (rename-in racket [string rkt:string])
         racket/function
         parsack

         (for-syntax racket/function))

(provide (all-defined-out)
         (all-from-out parsack racket))

(define-syntax (parser-rep stx)
  (syntax-case stx ()
    [(_ n parser) ; =>
     (let ((reps (build-list (syntax->datum #'n) (const #'parser))))
       #`(parser-seq #,@reps))]))

(define ret-number (compose string->number list->string))

; parsack's $space and $spaces use the predicate char-whitespace?
; which encompasses ALL whitespace chars.
; Instead, want only to eat HORIZONTAL whitespace (unicode class Zs or #\tab)
(define $blank (<?> (satisfy char-blank?) "horizontal space"))
(define $blanks (<?> (skipMany $blank) "horizontal white space"))
