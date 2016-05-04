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

; List is monoid
; (null) is mzero
(define (list/mconcat xs)
  (foldr (λ (x a)
                          (if (not (eq? null (car x)))
                              (cons (car x) a)
                              a))
                        '() xs))

; parsack's $space and $spaces use the predicate char-whitespace?
; which encompasses ALL whitespace chars.
; Instead, want only to eat HORIZONTAL whitespace (unicode class Zs or #\tab)
(define $blank (<?> (satisfy char-blank?) "horizontal space"))
(define $blanks (<?> (skipMany $blank) "horizontal white space"))
