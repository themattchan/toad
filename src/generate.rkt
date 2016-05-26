#lang racket

; TODO: some method of reading a site structure from config, hardcode this for now
; TODO: add organisational methods, i.e. by d/m/y, m/d/y, tags, etc

(define config-file "toad_config.sx")

(define (is-toad-root-dir?)
  (file-exists? (build-path (current-directory) config-file)))

(define (compile-post loc) ...)

(define (compile-blog [with-drafts #f])
  (let* ((blog-src (build-path (current-directory) "_posts"))
	 (posts (directory-list blog-src)))

    ;; todo: filter / splice list of drafts
    (map compile-post posts)))
	 
	

